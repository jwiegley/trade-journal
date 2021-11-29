{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Journal.Gains
  ( Calculation (..),
    ActionLike (..),
    gains,
  )
where

import Amount
import Control.Applicative
import Control.Arrow ((***))
import Control.Lens
import Control.Monad.State
import Data.Foldable
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics hiding (to)
import Journal.Split
import Journal.Types
  ( Annotated,
    Annotation (Idents),
    Disposition (..),
    Lot,
    details,
    fees,
    item,
    price,
    symbol,
  )
-- import Journal.Types hiding
--   ( Action,
--     _Buy,
--     _Close,
--     _Open,
--     _Sell,
--   )
import Pipes
import Text.Show.Pretty hiding (Time)
import Prelude hiding (Double, Float)

class ActionLike a where
  _Buy :: Prism' a Lot
  _Sell :: Prism' a Lot

  _Open :: Prism' a (Disposition, Lot)
  _Close :: Prism' a (Disposition, Lot, Amount 6)

  buyOrSell :: Traversal' a Lot
  buyOrSell = failing _Buy _Sell

  symbolName :: Traversal' a Text
  symbolName f s = s & buyOrSell . symbol %%~ f

data Calculation = FIFO | LIFO
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal
    )

data BasicState a = BasicState
  { _calc :: Calculation,
    _nextId :: Int,
    _events :: a
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      Functor,
      Foldable,
      Traversable
    )

makeLenses ''BasicState

data Opened = Opened
  { _openLong :: Bool,
    _openIds :: [Int],
    _openLot :: Lot
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal
    )

makeLenses ''Opened

type GainsState = BasicState (Map Text [Annotated Opened])

newGainsState :: Calculation -> GainsState
newGainsState c =
  BasicState
    { _calc = c,
      _nextId = 0,
      _events = mempty
    }

type LocalState = BasicState [Annotated Opened]

localState :: Text -> Traversal' GainsState LocalState
localState instrument f s =
  f (view (at instrument . non []) <$> s) <&> \m ->
    s & nextId .~ (m ^. nextId)
      & events . at instrument ?~ (m ^. events)

gains ::
  (ActionLike a, Functor m) =>
  Calculation ->
  Pipe (Annotated a) (Annotated a) m r
gains c = flip evalStateT (newGainsState c) $
  forever $ do
    entry <- lift await
    gst <- get
    let (results, gst') =
          flip runState gst $
            zoom (localState (entry ^. item . symbolName)) $
              untilDone handle entry
    put gst'
    forM_ results $ lift . yield

handle ::
  ActionLike a =>
  Annotated a ->
  State
    LocalState
    ([Annotated a], Remainder (Annotated a))
handle ann@(has (item . buyOrSell) -> True) =
  use events >>= \case
    open@(preview (item . openLong) -> Just buyToOpen) : rest
      | buyToOpen == has (item . _Sell) ann -> do
        events .= rest
        closePosition open ann
    _ -> (,Finished) <$> openPosition ann
handle x = pure ([x], Finished)

-- | Open a new position.
openPosition ::
  ActionLike a =>
  Annotated a ->
  State LocalState [Annotated a]
openPosition open = do
  nextId += 1
  ident <- use nextId
  let event =
        Opened
          { _openLong = has (item . _Buy) open,
            _openIds = [ident],
            _openLot = open ^?! item . buyOrSell
          }
          <$ open
  use calc >>= \case
    FIFO -> events <>= [event]
    LIFO -> events %= (event :)
  pure
    [ open,
      --
      _Open
        # ( if has (item . _Buy) open
              then Long
              else Short,
            open ^?! item . buyOrSell
          )
        <$ open
        & details <>~ [Idents [ident]]
    ]

-- | Close an existing position. If the amount to close is more than what is
--   open, the remainder is considered a separate opening event.
closePosition ::
  ActionLike a =>
  Annotated Opened ->
  Annotated a ->
  State LocalState ([Annotated a], Remainder (Annotated a))
closePosition open close =
  alignForClose
    (open ^?! item . openLot)
    (close ^?! item . buyOrSell)
    ( \su du -> do
        let pl
              | has (item . _Sell) close =
                du ^. price - su ^. price
              | otherwise =
                su ^. price - du ^. price
            lotFees = sum (close ^.. fees) + sum (open ^.. fees)
        pure
          [ close & item . buyOrSell .~ du,
            --
            _Close
              # ( if has (item . _Sell) close
                    then Long
                    else Short,
                  su,
                  pl - lotFees
                )
              <$ close
              & details <>~ [Idents (open ^?! item . openIds)]
          ]
    )
    (\sk -> events %= ((open & item . openLot .~ sk) :))
    (\dk -> pure $ close & item . buyOrSell .~ dk)

untilDone :: Monad m => (a -> m ([a], Remainder a)) -> a -> m [a]
untilDone f = go
  where
    go =
      f >=> \(results, remaining) ->
        case remaining of
          Finished -> pure results
          Remainder r -> (results ++) <$> go r

alignForClose ::
  (Splittable n a, Splittable n b, Applicative m) =>
  a ->
  b ->
  (a -> b -> m [x]) ->
  (a -> m ()) ->
  (b -> m z) ->
  m ([x], Remainder z)
alignForClose l r f g h =
  alignedA l r f g h
    <&> ( fromMaybe []
            *** \case
              Remainder (Right r') -> Remainder r'
              _ -> Finished
        )

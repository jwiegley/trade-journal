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
    GainsEvent (..),
    ActionLike (..),
    gains,
  )
where

import Amount
import Control.Applicative
import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.State
import Data.Foldable
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics hiding (to)
import Journal.Split
import Journal.Types hiding
  ( Action,
    _Buy,
    _Close,
    _Open,
    _Sell,
  )
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

data GainsEvent
  = Opened Bool [Int] Lot
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal
    )

makePrisms ''GainsEvent

openIds :: Traversal' GainsEvent [Int]
openIds = _Opened . _2

opened :: Traversal' GainsEvent Lot
opened = _Opened . _3

type GainsState = BasicState (Map Text [Annotated GainsEvent])

newGainsState :: Calculation -> GainsState
newGainsState c =
  BasicState
    { _calc = c,
      _nextId = 0,
      _events = mempty
    }

type LocalState = BasicState [Annotated GainsEvent]

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
    results <-
      zoom (localState (entry ^. item . symbolName)) $
        untilDone handle entry
    forM_ results $ lift . yield

handle ::
  (ActionLike a, Monad m) =>
  Annotated a ->
  StateT
    LocalState
    m
    ([Annotated a], Remainder (Annotated a))
handle ann@(has (item . buyOrSell) -> True) =
  use events >>= \case
    open@(preview (item . _Opened . _1) -> Just buyToOpen) : rest
      | buyToOpen == has (item . _Sell) ann ->
        closePosition open rest ann
    _ -> (,Finished) <$> openPosition ann
handle x = pure ([x], Finished)

-- | Open a new position.
openPosition ::
  (ActionLike a, Monad m) =>
  Annotated a ->
  StateT LocalState m [Annotated a]
openPosition open = do
  nextId += 1
  ident <- use nextId
  let event =
        Opened
          (has (item . _Buy) open)
          [ident]
          (open ^?! item . buyOrSell)
          <$ open
  use calc >>= \case
    FIFO -> events <>= [event]
    LIFO -> events %= (event :)
  pure
    [ open,
      ( ( _Open
            # ( if has (item . _Buy) open
                  then Long
                  else Short,
                open ^?! item . buyOrSell
              )
        )
          <$ open
      )
        & details <>~ [Idents [ident]]
    ]

-- | Close an existing position. If the amount to close is more than what is
--   open, the remainder is considered a separate opening event.
closePosition ::
  (ActionLike a, Monad m) =>
  Annotated GainsEvent ->
  [Annotated GainsEvent] ->
  Annotated a ->
  StateT LocalState m ([Annotated a], Remainder (Annotated a))
closePosition open rest close = do
  events .= []
  alignForClose
    (open ^?! item . opened)
    (close ^?! item . buyOrSell)
    ( \su du -> do
        let lotFees =
              sum (close ^.. fees) + sum (open ^.. fees)
            pricing o x =
              x ^. price
                + sum (o ^.. details . traverse . _Washed)
            pl
              | has (item . _Sell) close =
                pricing close du - pricing open su - lotFees
              | otherwise =
                pricing open su - pricing close du - lotFees
        pure
          [ close & item . buyOrSell .~ du,
            ( _Close
                # ( if has (item . _Sell) close
                      then Long
                      else Short,
                    su,
                    pl
                  )
                <$ close
            )
              & details <>~ [Idents (open ^?! item . openIds)]
          ]
    )
    (\sk -> events .= [open & item . opened .~ sk])
    (\dk -> pure $ close & item . buyOrSell .~ dk)
    <* (events <>= rest)

data Remainder a = Remainder a | Finished
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

untilDone :: Monad m => (a -> m ([a], Remainder a)) -> a -> m [a]
untilDone f = go
  where
    go entry = do
      (results, remaining) <- f entry
      case remaining of
        Finished -> pure results
        Remainder entry' ->
          (results ++) <$> go entry'

alignForClose ::
  (Splittable n a, Splittable n b, Applicative m) =>
  a ->
  b ->
  (a -> b -> m [x]) ->
  (a -> m ()) ->
  (b -> m z) ->
  m ([x], Remainder z)
alignForClose l r f g h =
  alignedA l r f g h <&> \(mresult, meremaining) ->
    ( fromMaybe [] mresult,
      case meremaining of
        Just (Right r') -> Remainder r'
        _ -> Finished
    )

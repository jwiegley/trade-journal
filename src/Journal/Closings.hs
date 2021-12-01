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

module Journal.Closings
  ( Calculation (..),
    closings,
  )
where

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
import Pipes
import Text.Show.Pretty hiding (Time)
import Prelude hiding (Double, Float)

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

type ClosingState = BasicState (Map Text [Annotated Entry])

newClosingState :: Calculation -> ClosingState
newClosingState c =
  BasicState
    { _calc = c,
      _nextId = 0,
      _events = mempty
    }

type LocalState = BasicState [Annotated Entry]

localState :: Text -> Traversal' ClosingState LocalState
localState instrument f s =
  f (view (at instrument . non []) <$> s) <&> \m ->
    s & nextId .~ (m ^. nextId)
      & events . at instrument ?~ (m ^. events)

closings ::
  Functor m =>
  Calculation ->
  Pipe
    (Maybe (Annotated Entry))
    (Annotated Entry)
    m
    (Map Text [Annotated Entry])
closings = evalStateT go . newClosingState
  where
    go = do
      mentry <- lift await
      case mentry of
        Nothing -> use events
        Just entry -> do
          lift $ yield entry
          gst <- get
          forM_ (entry ^? item . _Action . buyOrSell . symbol) $ \sym -> do
            let (results, gst') =
                  flip runState gst $
                    zoom (localState sym) $
                      untilDone handle entry
            put gst'
            forM_ results $ lift . yield
          go

untilDone :: Monad m => (a -> m ([a], Remainder a)) -> a -> m [a]
untilDone f = go
  where
    go =
      f >=> \(results, remaining) ->
        case remaining of
          Finished -> pure results
          Remainder r -> (results ++) <$> go r

handle ::
  Annotated Entry ->
  State
    LocalState
    ([Annotated Entry], Remainder (Annotated Entry))
handle ann@(has (item . _Action . buyOrSell) -> True) =
  use events >>= \case
    open@(preview (item . _Event . _Open . posDisp) -> Just disp) : rest
      | disp
          == if has (item . _Action . _Sell) ann
            then Long
            else Short -> do
        events .= rest
        closePosition open ann
    _ -> (,Finished) <$> openPosition ann
handle _ = pure ([], Finished)

-- | Open a new position.
openPosition ::
  Annotated Entry ->
  State LocalState [Annotated Entry]
openPosition open = do
  nextId += 1
  ident <- use nextId
  let lot = open ^?! item . _Action . buyOrSell
      event =
        _Event
          # _Open
          # Position
            { _posIdent = ident,
              _posLot = lot,
              _posDisp =
                if has (item . _Action . _Buy) open
                  then Long
                  else Short,
              _posBasis = lot ^. price,
              _posWash = []
            }
          <$ open
  use calc >>= \case
    FIFO -> events <>= [event]
    LIFO -> events %= (event :)
  pure [event]

-- | Close an existing position. If the amount to close is more than what is
--   open, the remainder is considered a separate opening event.
closePosition ::
  Annotated Entry ->
  Annotated Entry ->
  State LocalState ([Annotated Entry], Remainder (Annotated Entry))
closePosition open close =
  let o = open ^?! item . _Event . _Open
   in alignForClose
        (o ^. posLot)
        (close ^?! item . _Action . buyOrSell)
        ( \_su du ->
            pure
              [ _Event
                  # _Close
                  # Closing
                    { _closingPos = o,
                      _closingLot = du,
                      _closingWash = []
                    }
                  <$ close
              ]
        )
        (\sk -> events %= ((open & item . _Event . _Open . posLot .~ sk) :))
        (\dk -> pure $ close & item . _Action . buyOrSell .~ dk)

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

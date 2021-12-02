{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Journal.Closings
  ( Calculation (..),
    closings,
    openPositions,
  )
where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad.State
import Data.Aeson hiding ((.=))
import Data.Foldable
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics hiding (to)
import Journal.Split
import Journal.Types
import Text.Show.Pretty hiding (Time)
import Prelude hiding (Double, Float)

data Calculation = FIFO | LIFO
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal,
      FromJSON
    )

data BasicState e = BasicState
  { _calc :: Calculation,
    _nextId :: Int,
    _events :: e
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

type ClosingState a = BasicState (Map Text [Annotated (Entry a)])

newClosingState :: Calculation -> ClosingState a
newClosingState c =
  BasicState
    { _calc = c,
      _nextId = 0,
      _events = mempty
    }

type LocalState a = BasicState [Annotated (Entry a)]

localState ::
  (Eq a, Monoid a) =>
  Text ->
  Traversal' (ClosingState a) (LocalState a)
localState instrument f s =
  f (view (at instrument . non mempty) <$> s) <&> \m ->
    s & nextId .~ (m ^. nextId)
      & events . at instrument ?~ (m ^. events)

openPositions ::
  (Monoid a, Eq a, Show a) =>
  Calculation ->
  [Annotated (Entry a)] ->
  Map Text [Annotated (Entry a)]
openPositions mode = flip execState mempty . go
  where
    remember ::
      (Monoid a, Eq a, Show a) =>
      Annotated (Entry a) ->
      Text ->
      State (Map Text [Annotated (Entry a)]) ()
    remember x sym = case mode of
      FIFO -> at sym . non mempty <>= [x]
      LIFO -> at sym . non mempty %= (x :)

    go ::
      (Monoid a, Eq a, Show a) =>
      [Annotated (Entry a)] ->
      State (Map Text [Annotated (Entry a)]) ()
    go [] = pure ()
    go (o@((^? opening) -> Just pos) : xs) = do
      remember o (pos ^. posLot . symbol)
      go xs
    go (c@((^? closing) -> Just cl) : xs) = do
      let sym = cl ^. closingLot . symbol
      preuse (ix sym . _head) >>= \case
        Nothing ->
          error $
            "Expected open position "
              ++ show (cl ^. closingPos . posIdent)
        Just o -> do
          ix sym %= tail
          case o ^? opening of
            Nothing ->
              error $ "Unexpected entry in open positions list " ++ show o
            Just p
              | p ^. posIdent /= cl ^. closingPos . posIdent ->
                error $
                  "Unexpected open position "
                    ++ show (p ^. posIdent)
                    ++ " /= "
                    ++ show (cl ^. closingPos . posIdent)
              | otherwise -> do
                (_, remainder) <-
                  alignedA
                    (p ^. posLot)
                    (cl ^. closingLot)
                    (\_ou _cu -> pure ())
                    (\ok -> remember (o & opening . posLot .~ ok) sym)
                    (\ck -> pure (c & closing . closingLot .~ ck))
                case remainder of
                  Remainder (Right x) -> go (x : xs)
                  _ -> go xs
    go (_ : xs) = go xs

closings ::
  (Eq a, Monoid a) =>
  Calculation ->
  [Annotated (Entry a)] ->
  ([Annotated (Entry a)], Map Text [Annotated (Entry a)])
closings mode =
  (concat *** _events) . flip runState (newClosingState mode) . mapM go
  where
    go entry = do
      gst <- get
      case entry ^? item . _Action . buyOrSell . symbol of
        Just sym -> do
          let (results, gst') =
                flip runState gst $
                  zoom (localState sym) $
                    untilDone handle entry
          put gst'
          pure (entry : results)
        Nothing -> pure [entry]

untilDone :: Monad m => (a -> m ([a], Remainder a)) -> a -> m [a]
untilDone f = go
  where
    go =
      f >=> \(results, remaining) ->
        case remaining of
          Finished -> pure results
          Remainder r -> (results ++) <$> go r

handle ::
  (Eq a, Monoid a) =>
  Annotated (Entry a) ->
  State
    (LocalState a)
    ([Annotated (Entry a)], Remainder (Annotated (Entry a)))
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
  (Eq a, Monoid a) =>
  Annotated (Entry a) ->
  State (LocalState a) [Annotated (Entry a)]
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
              _posData = mempty
            }
          <$ open
  use calc >>= \case
    FIFO -> events <>= [event]
    LIFO -> events %= (event :)
  pure [event]

-- | Close an existing position. If the amount to close is more than what is
--   open, the remainder is considered a separate opening event.
closePosition ::
  (Eq a, Monoid a) =>
  Annotated (Entry a) ->
  Annotated (Entry a) ->
  State (LocalState a) ([Annotated (Entry a)], Remainder (Annotated (Entry a)))
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
                      _closingData = mempty
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

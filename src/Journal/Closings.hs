{-# LANGUAGE BlockArguments #-}
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
    positions,
    positionsFromEntry,
  )
where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad.State
import Data.Aeson hiding ((.=))
import Data.Foldable
import Data.IntMap (IntMap)
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

type ClosingState a = BasicState (Map Text (IntMap (Annotated (Entry a))))

newClosingState :: Calculation -> ClosingState a
newClosingState c =
  BasicState
    { _calc = c,
      _nextId = 0,
      _events = mempty
    }

type LocalState a = BasicState (IntMap (Annotated (Entry a)))

localState ::
  (Eq a, Monoid a) =>
  Text ->
  Traversal' (ClosingState a) (LocalState a)
localState instrument f s =
  f (view (at instrument . non mempty) <$> s) <&> \m ->
    s & nextId .~ (m ^. nextId)
      & events . at instrument ?~ (m ^. events)

closings ::
  (Eq a, Monoid a) =>
  Calculation ->
  [Annotated (Entry a)] ->
  ([Annotated (Entry a)], Map Text (IntMap (Annotated (Entry a))))
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
handle ann@(has (item . _Action . buyOrSell) -> True) = do
  mode <- use calc
  -- jww (2021-12-04): the buy/sell should be able to specify FIFO or LIFO,
  -- and the user should be able to set it as a default. In the case of LIFE,
  -- this traversal needs to be reversed.
  gets
    ( (case mode of FIFO -> id; LIFO -> reverse)
        . (^.. events . traverse)
    )
    >>= \case
      open@(preview (opening . posDisp) -> Just disp) : _
        | disp
            == if has (item . _Action . _Sell) ann
              then Long
              else Short -> do
          events . at (open ^?! opening . posIdent) .= Nothing
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
              _posData = mempty
            }
          <$ open
  events . at ident ?= event
  pure [event]

-- | Close an existing position. If the amount to close is more than what is
--   open, the remainder is considered a separate opening event.
closePosition ::
  (Eq a, Monoid a) =>
  Annotated (Entry a) ->
  Annotated (Entry a) ->
  State (LocalState a) ([Annotated (Entry a)], Remainder (Annotated (Entry a)))
closePosition open close =
  let o = open ^?! opening
   in alignForClose
        (o ^. posLot)
        (close ^?! item . _Action . buyOrSell)
        ( \_su du ->
            pure
              [ _Event
                  # _Close
                  # Closing
                    { _closingIdent = o ^. posIdent,
                      _closingLot = du,
                      _closingData = mempty
                    }
                  <$ close
              ]
        )
        ( \sk ->
            events . at (open ^?! opening . posIdent)
              ?= (open & opening . posLot .~ sk)
        )
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
    <&> fromMaybe []
      *** \case
        Remainder (Right r') -> Remainder r'
        _ -> Finished

positions ::
  (Monoid a, Eq a, Show a) =>
  [Annotated (Entry a)] ->
  Map Text (IntMap (Annotated (Entry a)))
positions = foldl' positionsFromEntry mempty

positionsFromEntry ::
  (Monoid a, Eq a, Show a) =>
  Map Text (IntMap (Annotated (Entry a))) ->
  Annotated (Entry a) ->
  Map Text (IntMap (Annotated (Entry a)))
positionsFromEntry m = go
  where
    go o@((^? opening) -> Just pos) =
      m
        & at (pos ^. posLot . symbol)
          . non mempty
          . at (pos ^. posIdent)
        ?~ o
    go ((^? closing) -> Just cl) =
      flip execState m do
        let loc ::
              Eq a =>
              Traversal'
                (Map Text (IntMap (Annotated (Entry a))))
                (Maybe (Annotated (Entry a)))
            loc =
              at (cl ^. closingLot . symbol)
                . non mempty
                . at (cl ^. closingIdent)
        preuse (loc . _Just) >>= \case
          Nothing ->
            error $
              "Attempt to close non-open position:\n"
                ++ ppShow cl
                ++ "\n---- against open positions ----\n"
                ++ ppShow m
          Just o ->
            alignedA
              (o ^?! opening . posLot)
              (cl ^. closingLot)
              (\_ou _cu -> loc .= Nothing)
              (\ok -> loc ?= (o & opening . posLot .~ ok))
              (\_ck -> error "Invalid closing")
    go _ = m

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-orphans #-}

module TestAction where

import Amount
import Closings hiding (positions)
import qualified Closings
import Control.Applicative
import Control.Arrow (second)
import Control.Exception
import Control.Lens hiding (Context)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ratio
import Data.Text (Text)
import Data.Time
import Data.Typeable
import GHC.Generics hiding (to)
import Hedgehog hiding (Action, assert)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Journal.Entry
import Journal.Pipes ()
import Journal.Types
import Process
import Test.HUnit.Lang (FailureReason (..))
import Test.Tasty.HUnit hiding (assert)
import Text.Show.Pretty hiding (Time)

{--------------------------------------------------------------------------}

newtype MockFailure = MockFailure FailureReason
  deriving (Eq, Typeable)

instance Show MockFailure where
  show (MockFailure (Reason msg)) = msg
  show (MockFailure (ExpectedButGot preface expected actual)) = msg
    where
      msg =
        (case preface of Just str -> str ++ "\n"; Nothing -> "")
          ++ "expected:\n"
          ++ expected
          ++ "\n but got:\n"
          ++ actual

instance Exception MockFailure

assertEqual' ::
  (Eq a, PrettyVal a, HasCallStack) =>
  -- | The message prefix
  String ->
  -- | The expected value
  a ->
  -- | The actual value
  a ->
  Assertion
assertEqual' preface expected actual =
  unless (actual == expected) $ do
    throwIO
      ( MockFailure
          ( ExpectedButGot
              ( if Prelude.null preface
                  then Nothing
                  else Just preface
              )
              (dumpStr expected)
              (dumpStr actual)
          )
      )

infix 1 @?==

(@?==) :: (MonadIO m, Eq a, PrettyVal a, HasCallStack) => a -> a -> m ()
actual @?== expected = liftIO $ assertEqual' "" expected actual

{--------------------------------------------------------------------------}

genUTCTime :: MonadGen m => m UTCTime
genUTCTime = do
  y <- toInteger <$> Gen.int (Range.constant 2000 2019)
  m <- Gen.int (Range.constant 1 12)
  d <- Gen.int (Range.constant 1 28)
  let day = fromGregorian y m d
  secs <- toInteger <$> Gen.int (Range.constant 0 86401)
  pure $ UTCTime day (secondsToDiffTime secs)

genAmount :: MonadGen m => Range Integer -> m (Amount n)
genAmount range = do
  d <- Gen.integral range
  n <- Gen.integral range
  pure $ Amount (d % n)

genAnnotated :: MonadGen m => m a -> m (Annotated a)
genAnnotated gen = do
  _time <- genUTCTime
  _item <- gen
  let _details = []
      _context =
        Context
          { _currency = "",
            _account = ""
          }
  pure Annotated {..}

genLot :: MonadGen m => m Lot
genLot = do
  q <- genAmount (Range.linear 1 1000)
  p <- genAmount (Range.linear 1 2000)
  pure $ Lot q "FOO" p

{--------------------------------------------------------------------------}

instance (PrettyVal a, PrettyVal b) => PrettyVal (Map a b) where
  prettyVal = Con "Map.fromList" . map prettyVal . M.assocs

instance PrettyVal a => PrettyVal (IntMap a) where
  prettyVal = Con "IntMap.fromList" . map prettyVal . IM.assocs

_Entity ::
  Prism' (Annotated ProcessedEntry) (Annotated Entry)
_Entity = prism' putTo getFrom
  where
    putTo ::
      Annotated Entry ->
      Annotated ProcessedEntry
    putTo x = AnEntry (x ^. item) <$ x

    getFrom :: Annotated ProcessedEntry -> Maybe (Annotated Entry)
    getFrom ent = case ent ^. item of
      AnEntry x -> Just (x <$ ent)
      APositionEvent _ -> Nothing
      AWashing _ -> Nothing

data Positions = Positions
  { _positions :: !(IntMap Position),
    _exprs :: ![Annotated ProcessedEntry]
  }

newPositions :: Positions
newPositions = Positions mempty mempty

makeLenses ''Positions

getExprs :: MonadIO m => StateT Positions m a -> m [Annotated ProcessedEntry]
getExprs act = view exprs <$> execStateT act newPositions

instance PrettyVal () where
  prettyVal () = String "()"

data ProcessedEntryClose = ProcessedEntryClose
  { _eClosePosition :: !Int,
    _eCloseLot :: !(Annotated Lot),
    _eClosePL :: !(Amount 6)
  }
  deriving (Show, Eq, Generic, PrettyVal, Typeable)

makeLenses ''ProcessedEntryClose

type TestDSL = State [Annotated ProcessedEntry]

handlePositionEvent ::
  MonadIO m =>
  PositionEvent ->
  StateT Positions m ()
handlePositionEvent (Open pos) = do
  -- traceM $ "write " ++ show (pos ^. posIdent) ++ " => " ++ ppShow pos
  positions . at (pos ^. posIdent) ?= pos
handlePositionEvent (Close cl) = do
  let n = cl ^. closingPos
  -- traceM $ "read " ++ show n
  preuse (positions . ix (cl ^. closingPos)) >>= \case
    Nothing -> error $ "No open position " ++ show n
    Just pos -> do
      -- traceM $ "... " ++ show pos
      let pos' = pos & posLot . amount -~ (cl ^. closingLot . amount)
          amt = pos' ^?! posLot . amount
      if amt < 0
        then error $ "Not enough shares in open position " ++ show n
        else
          if amt == 0
            then positions . at n .= Nothing
            else positions . at n ?= pos'

eval :: MonadIO m => Annotated ProcessedEntry -> StateT Positions m ()
eval x@(Annotated (AnEntry _) _ _ _) = do
  exprs <>= [x]
eval x@(Annotated (AWashing _) _ _ _) = do
  exprs <>= [x]
eval x@(Annotated (APositionEvent p) _ _ _) = do
  exprs <>= [x]
  handlePositionEvent p

evalDSL :: MonadIO m => TestDSL () -> StateT Positions m ()
evalDSL = mapM_ TestAction.eval . flip execState []

buy :: Annotated Lot -> TestDSL ()
buy b =
  id
    <>= [ AnEntry
            ( TradeEntry
                ( Trade
                    { _tradeAction = Buy,
                      _tradeLot = b ^. item,
                      _tradeFees = Fees 0 0
                    }
                )
            )
            <$ b
        ]

sell :: Annotated Lot -> TestDSL ()
sell s =
  id
    <>= [ AnEntry
            ( TradeEntry
                ( Trade
                    { _tradeAction = Sell,
                      _tradeLot = s ^. item,
                      _tradeFees = Fees 0 0
                    }
                )
            )
            <$ s
        ]

open ::
  Int ->
  Disposition ->
  Annotated Lot ->
  TestDSL ()
open i disp b =
  id
    <>= [ APositionEvent
            ( Open
                Position
                  { _posIdent = i,
                    _posLot = b ^. item,
                    _posDisp = disp
                  }
            )
            <$ b
        ]

close ::
  Int ->
  Annotated Lot ->
  TestDSL ()
close n b = id <>= [APositionEvent (Close (Closing n (b ^. item))) <$ b]

{--------------------------------------------------------------------------}

class PrettyVal1 f where
  liftPrettyVal :: (a -> Value) -> f a -> Value

instance PrettyVal e => PrettyVal1 (Const e) where
  liftPrettyVal _ (Const x) = prettyVal x

checkJournal ::
  MonadIO m =>
  ( [Annotated Entry] ->
    ( [Annotated ProcessedEntry],
      Map Text (IntMap (Annotated PositionEvent))
    )
  ) ->
  TestDSL () ->
  TestDSL () ->
  TestDSL () ->
  m ()
checkJournal f journal act actLeft =
  do
    journal' <- getExprs (evalDSL journal)
    expectedEntries <- getExprs (evalDSL act)
    expectedOpenPositions <-
      Closings.positions . positionsOnly <$> getExprs (evalDSL actLeft)
    assert (length journal' == length (entriesOnly journal')) $
      f (entriesOnly journal') @?== (expectedEntries, expectedOpenPositions)

checkJournal' ::
  MonadIO m =>
  Bool ->
  ( ( [Annotated ProcessedEntry],
      Map Text (IntMap (Annotated PositionEvent))
    ) ->
    ( [Annotated ProcessedEntry],
      Map Text (IntMap (Annotated PositionEvent))
    )
  ) ->
  TestDSL () ->
  TestDSL () ->
  TestDSL () ->
  m ()
checkJournal' washSales f =
  checkJournal
    ( second
        ( \entriesLeft -> case entriesLeft ^? ix "FOO" of
            Just m
              | IM.null m ->
                  entriesLeft & at "FOO" .~ Nothing
            _ -> entriesLeft
        )
        . f
        . processJournal FIFO washSales
    )

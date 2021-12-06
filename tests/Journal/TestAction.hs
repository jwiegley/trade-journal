{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-orphans #-}

module TestAction where

import Amount
import Control.Exception
import Control.Lens hiding (each)
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
-- import Debug.Trace
import GHC.Generics hiding (to)
import Hedgehog hiding (Action)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Journal.Closings hiding (positions)
import qualified Journal.Closings as Closings
import Journal.Pipes ()
import Journal.Types
import Test.HUnit.Lang (FailureReason (..))
import Test.Tasty.HUnit
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

data Positions a = Positions
  { _positions :: IntMap (Position a),
    _entries :: [Annotated (Entry a)]
  }

newPositions :: Positions a
newPositions = Positions mempty mempty

makeLenses ''Positions

getEntries :: MonadIO m => StateT (Positions a) m r -> m [Annotated (Entry a)]
getEntries act = view entries <$> execStateT act newPositions

instance PrettyVal () where
  prettyVal () = String "()"

data TestExprClose a = TestExprClose
  { _eClosePosition :: Int,
    _eCloseLot :: Annotated Lot,
    _eClosePL :: Amount 6,
    _eCloseData :: a
  }
  deriving (Show, Eq, Generic, PrettyVal, Typeable)

makeLenses ''TestExprClose

data TestExpr a
  = EBuy (Annotated Lot)
  | ESell (Annotated Lot)
  | EOpen (Annotated (Position a))
  | EClose (TestExprClose a)
  deriving (Show, Eq, Generic, PrettyVal, Typeable)

makePrisms ''TestExpr

type TestDSL a = State [TestExpr a]

eval :: (Show a, MonadIO m) => TestExpr a -> StateT (Positions a) m ()
eval (EBuy b) = entries <>= [Buy <$> b]
eval (ESell s) = entries <>= [Sell <$> s]
eval (EOpen p@(view item -> pos)) = do
  positions . at (pos ^. posIdent) ?= pos
  -- traceM $ "write " ++ show (pos ^. posIdent) ++ " => " ++ ppShow pos
  entries <>= [Open pos <$ p]
eval (EClose (TestExprClose n s pl w)) = do
  preuse (positions . ix n) >>= \case
    Nothing -> error $ "No open position " ++ show n
    Just pos -> do
      -- traceM $ "read " ++ show n ++ " => " ++ ppShow pos
      let pl' = case pos ^. posDisp of
            Long -> (s ^. item . price) - (pos ^. posLot . price)
            Short -> (pos ^. posLot . price) - (s ^. item . price)
      liftIO $
        assertEqual'
          ("closing gain/loss for " ++ ppShow pos ++ " and " ++ ppShow s)
          pl
          pl'
      entries <>= [Close (Closing n (s ^. item) w) <$ s]
      -- entries' <- use entries
      -- traceM $ "entries' = " ++ ppShow entries'
      let pos' = pos & posLot . amount -~ (s ^. item . amount)
          amt = pos' ^?! posLot . amount
      if amt < 0
        then error $ "Not enough shares in open position " ++ show n
        else
          if amt == 0
            then positions . at n .= Nothing
            else positions . at n ?= pos'

evalDSL :: (Show a, MonadIO m) => TestDSL a () -> StateT (Positions a) m ()
evalDSL = mapM_ TestAction.eval . flip execState []

buy :: Annotated Lot -> TestDSL a ()
buy b = id <>= [EBuy b]

sell :: Annotated Lot -> TestDSL a ()
sell s = id <>= [ESell s]

open ::
  Monoid a =>
  Int ->
  Disposition ->
  Annotated Lot ->
  TestDSL a ()
open i disp b =
  id
    <>= [ EOpen $
            Position
              { _posIdent = i,
                _posLot = b ^. item,
                _posDisp = disp,
                _posData = mempty
              }
              <$ b
        ]

close ::
  Monoid a =>
  Int ->
  Annotated Lot ->
  Amount 6 ->
  TestDSL a ()
close n b pl = id <>= [EClose (TestExprClose n b pl mempty)]

{--------------------------------------------------------------------------}

checkJournal ::
  (Monoid a, Eq a, Show a, PrettyVal a, MonadIO m) =>
  ( ([Annotated (Entry a)], Map Text (IntMap (Annotated (Entry a)))) ->
    ([Annotated (Entry a)], Map Text (IntMap (Annotated (Entry a))))
  ) ->
  TestDSL a () ->
  TestDSL a () ->
  TestDSL a () ->
  m ()
checkJournal f journal act actLeft =
  do
    journal' <- getEntries (evalDSL journal)
    expectedEntries <- getEntries (evalDSL act)
    expectedEntriesLeft <- Closings.positions <$> getEntries (evalDSL actLeft)
    let (entries', entriesLeft') = f (closings FIFO journal')
        entriesLeft'' = case entriesLeft' ^? ix "FOO" of
          Just m
            | IM.null m ->
              entriesLeft' & at "FOO" .~ Nothing
          _ -> entriesLeft'
    (entries', entriesLeft'') @?== (expectedEntries, expectedEntriesLeft)

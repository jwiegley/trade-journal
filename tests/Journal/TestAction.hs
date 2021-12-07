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
import Data.Sum
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
import Journal.SumLens
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

data Positions r = Positions
  { _positions :: IntMap Position,
    _entries :: [Annotated (Sum r ())]
  }

newPositions :: Positions r
newPositions = Positions mempty mempty

makeLenses ''Positions

getEntries :: MonadIO m => StateT (Positions r) m a -> m [Annotated (Sum r ())]
getEntries act = view entries <$> execStateT act newPositions

instance PrettyVal () where
  prettyVal () = String "()"

data TestExprClose = TestExprClose
  { _eClosePosition :: Int,
    _eCloseLot :: Annotated Lot,
    _eClosePL :: Amount 6
  }
  deriving (Show, Eq, Generic, PrettyVal, Typeable)

makeLenses ''TestExprClose

data TestExpr r
  = Const Entry :< r => EBuy (Annotated Lot)
  | Const Entry :< r => ESell (Annotated Lot)
  | Const PositionEvent :< r => EOpen (Annotated Position)
  | Const PositionEvent :< r => EClose TestExprClose

deriving instance Show (TestExpr r)

-- Eq, Generic, PrettyVal)

makePrisms ''TestExpr

type TestDSL r = State [TestExpr r]

eval :: MonadIO m => TestExpr r -> StateT (Positions r) m ()
eval (EBuy b) = entries <>= [(projectedC . _Buy #) <$> b]
eval (ESell s) = entries <>= [(projectedC . _Sell #) <$> s]
eval (EOpen p@(view item -> pos)) = do
  positions . at (pos ^. posIdent) ?= pos
  -- traceM $ "write " ++ show (pos ^. posIdent) ++ " => " ++ ppShow pos
  entries <>= [projectedC . _Open # pos <$ p]
eval (EClose (TestExprClose n s pl)) = do
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
      entries <>= [projectedC . _Close # Closing n (s ^. item) <$ s]
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

evalDSL :: MonadIO m => TestDSL r () -> StateT (Positions r) m ()
evalDSL = mapM_ TestAction.eval . flip execState []

buy :: Const Entry :< r => Annotated Lot -> TestDSL r ()
buy b = id <>= [EBuy b]

sell :: Const Entry :< r => Annotated Lot -> TestDSL r ()
sell s = id <>= [ESell s]

open ::
  Const PositionEvent :< r =>
  Int ->
  Disposition ->
  Annotated Lot ->
  TestDSL r ()
open i disp b =
  id
    <>= [ EOpen $
            Position
              { _posIdent = i,
                _posLot = b ^. item,
                _posDisp = disp
              }
              <$ b
        ]

close ::
  Const PositionEvent :< r =>
  Int ->
  Annotated Lot ->
  Amount 6 ->
  TestDSL r ()
close n b pl = id <>= [EClose (TestExprClose n b pl)]

{--------------------------------------------------------------------------}

class PrettyVal1 f where
  liftPrettyVal :: (a -> Value) -> f a -> Value

instance PrettyVal e => PrettyVal1 (Const e) where
  liftPrettyVal _ (Const x) = prettyVal x

instance Apply PrettyVal1 fs => PrettyVal1 (Sum fs) where
  liftPrettyVal f = apply @PrettyVal1 (liftPrettyVal f)

instance (Apply PrettyVal1 t, PrettyVal v) => PrettyVal (Sum t v) where
  prettyVal = liftPrettyVal prettyVal

checkJournal ::
  forall s m.
  ( Const Entry :< s,
    Const PositionEvent :< s,
    Apply Show1 s,
    Apply Eq1 s,
    Apply PrettyVal1 s,
    MonadIO m
  ) =>
  ( ( [Annotated (Sum '[Const PositionEvent, Const Entry] ())],
      Map Text (IntMap (Annotated (Sum '[Const PositionEvent, Const Entry] ())))
    ) ->
    ( [Annotated (Sum s ())],
      Map Text (IntMap (Annotated (Sum s ())))
    )
  ) ->
  TestDSL '[Const Entry] () ->
  TestDSL s () ->
  TestDSL s () ->
  m ()
checkJournal f journal act actLeft =
  do
    (journal' :: [Annotated (Sum r ())]) <- getEntries (evalDSL journal)
    expectedEntries <- getEntries (evalDSL act)
    expectedEntriesLeft <- Closings.positions <$> getEntries (evalDSL actLeft)
    let (entries', entriesLeft') = f (closings FIFO journal')
        entriesLeft'' = case entriesLeft' ^? ix "FOO" of
          Just m
            | IM.null m ->
              entriesLeft' & at "FOO" .~ Nothing
          _ -> entriesLeft'
    (entries', entriesLeft'') @?== (expectedEntries, expectedEntriesLeft)

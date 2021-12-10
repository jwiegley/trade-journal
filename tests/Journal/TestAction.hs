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
import Data.Sum.Lens
import Data.Text (Text)
import Data.Time
import Data.Typeable
import GHC.Generics hiding (to)
import Hedgehog hiding (Action)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Journal.Closings hiding (positions)
import qualified Journal.Closings as Closings
import Journal.Entry.Deposit
import Journal.Entry.Income
import Journal.Entry.Options
import Journal.Entry.Trade
import Journal.Pipes ()
import Journal.Types
import Taxes.USA.WashSaleRule
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
  let _account = ""
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
  = Const Deposit :< r => ADeposit (Annotated Deposit)
  | Const Income :< r => AnIncome (Annotated Income)
  | Const Options :< r => AnOptions (Annotated Options)
  | Const Trade :< r => ATrade (Annotated Trade)
  | Const PositionEvent :< r => APositionEvent (Annotated PositionEvent)
  | Const Washing :< r => AWashing (Annotated Washing)

deriving instance Show (TestExpr r)

makePrisms ''TestExpr

type TestDSL r = State [TestExpr r]

handlePositionEvent ::
  (MonadIO m, HasPositionEvent f) =>
  f a ->
  StateT (Positions r) m ()
handlePositionEvent ((^? _Event . _Open) -> Just pos) = do
  -- traceM $ "write " ++ show (pos ^. posIdent) ++ " => " ++ ppShow pos
  positions . at (pos ^. posIdent) ?= pos
handlePositionEvent ((^? _Event . _Close) -> Just cl) = do
  let n = cl ^. closingIdent
  -- traceM $ "read " ++ show n
  preuse (positions . ix (cl ^. closingIdent)) >>= \case
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
handlePositionEvent _ = pure ()

eval :: MonadIO m => TestExpr r -> StateT (Positions r) m ()
eval (ADeposit b) = do
  entries <>= [(projectedC #) <$> b]
  handlePositionEvent (Const (b ^. item))
eval (AnIncome b) = do
  entries <>= [(projectedC #) <$> b]
  handlePositionEvent (Const (b ^. item))
eval (AnOptions b) = do
  entries <>= [(projectedC #) <$> b]
  handlePositionEvent (Const (b ^. item))
eval (ATrade b) = do
  entries <>= [(projectedC #) <$> b]
  handlePositionEvent (Const (b ^. item))
eval (AWashing w) = do
  entries <>= [(projectedC #) <$> w]
  handlePositionEvent (Const (w ^. item))
eval (APositionEvent p) = do
  entries <>= [(projectedC #) <$> p]
  handlePositionEvent (Const (p ^. item))

evalDSL :: MonadIO m => TestDSL r () -> StateT (Positions r) m ()
evalDSL = mapM_ TestAction.eval . flip execState []

buy :: Const Trade :< r => Annotated Lot -> TestDSL r ()
buy b =
  id
    <>= [ ATrade
            ( Trade
                { _tradeAction = Buy,
                  _tradeLot = b ^. item,
                  _tradeFees = 0,
                  _tradeCommission = 0
                }
                <$ b
            )
        ]

sell :: Const Trade :< r => Annotated Lot -> TestDSL r ()
sell s =
  id
    <>= [ ATrade
            ( Trade
                { _tradeAction = Sell,
                  _tradeLot = s ^. item,
                  _tradeFees = 0,
                  _tradeCommission = 0
                }
                <$ s
            )
        ]

open ::
  Const PositionEvent :< r =>
  Int ->
  Disposition ->
  Annotated Lot ->
  TestDSL r ()
open i disp b =
  id
    <>= [ APositionEvent
            ( Open
                Position
                  { _posIdent = i,
                    _posLot = b ^. item,
                    _posDisp = disp
                  }
                <$ b
            )
        ]

close ::
  Const PositionEvent :< r =>
  Int ->
  Annotated Lot ->
  TestDSL r ()
close n b = id <>= [APositionEvent (Close (Closing n (b ^. item)) <$ b)]

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
  ( '[Const Trade, Const Deposit, Const Income, Const Options] :<: s,
    HasTraversal' HasPositionEvent s,
    Const PositionEvent :< s,
    Apply Show1 s,
    Apply Eq1 s,
    Apply PrettyVal1 s,
    MonadIO m
  ) =>
  ( ( [ Annotated
          ( Sum
              '[ Const PositionEvent,
                 Const Trade,
                 Const Deposit,
                 Const Income,
                 Const Options
               ]
              ()
          )
      ],
      Map
        Text
        ( IntMap
            ( Annotated
                ( Sum
                    '[ Const PositionEvent,
                       Const Trade,
                       Const Deposit,
                       Const Income,
                       Const Options
                     ]
                    ()
                )
            )
        )
    ) ->
    ( [Annotated (Sum s ())],
      Map Text (IntMap (Annotated (Sum s ())))
    )
  ) ->
  TestDSL '[Const Trade, Const Deposit, Const Income, Const Options] () ->
  TestDSL s () ->
  TestDSL s () ->
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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module GainsKeeper where

{-
import Amount
import Control.Exception
import Control.Lens hiding (each)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Ratio
import Data.Time
import Data.Typeable
import Hedgehog hiding (Action)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Journal.GainsKeeper
import Journal.Pipes ()
import Journal.Types
import Pipes
import qualified Pipes.Prelude as P
import Test.HUnit.Lang (FailureReason (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Text.Show.Pretty hiding (Time)

{--------------------------------------------------------------------------}

data TestAction = TestAction
  { _action :: Annotated Entry,
    _annotations :: [Annotation],
    _regular :: Bool
  }

makeLenses ''TestAction

data TestState = TestState
  { _pending :: [TestAction],
    _balance :: Amount 2
  }

makeLenses ''TestState

buy :: Annotated Lot -> State TestState ()
buy b =
  do
    -- bal <- use balance
    balance += c
    pending
      <>= [ TestAction
              (Action . Buy <$> b)
              []
              True
          ]
  where
    c = ((b ^. item . amount) * (b ^. item . price)) ^. coerced . to negate

sell :: Annotated Lot -> State TestState ()
sell b =
  do
    -- bal <- use balance
    balance += c
    pending
      <>= [ TestAction
              (Action . Sell <$> b)
              []
              True
          ]
  where
    c = ((b ^. item . amount) * (b ^. item . price)) ^. coerced

wash :: Annotated Lot -> State TestState ()
wash b =
  do
    -- bal <- use balance
    pending
      <>= [ TestAction
              (Event . Wash Future (b ^. time) <$> (b & item . price .~ 1))
              []
              False
          ]

input :: State TestState a -> [Annotated Entry]
input =
  map _action
    . filter _regular
    . _pending
    . flip execState (TestState [] 0)

trades :: State TestState a -> [Annotated Entry]
trades =
  map (\x -> (x ^. action) & details <>~ (x ^. annotations))
    . _pending
    . flip execState (TestState [] 0)

checkJournal ::
  MonadIO m =>
  State TestState a ->
  [Annotated (Change Entry)] ->
  m ()
checkJournal journal expected =
  P.toListM (each (input journal) >-> gainsKeeper)
    @?== Right expected

{--------------------------------------------------------------------------}

testGainsKeeper :: TestTree
testGainsKeeper =
  testGroup
    "gainskeeper"
    [ testProperty "simple-buy" simpleBuy,
      testProperty "buy-buy" buyBuy,
      testProperty "buy-sell-breakeven" buySellBreakeven,
      testProperty "buy-sell-profit" buySellProfit,
      testProperty "buy-sell-part-profit" buySellPartProfit,
      testProperty "buy-buy-sell-loss" buyBuySellLoss,
      testProperty "buy-sell-loss-buy" buySellLossBuy,
      testProperty "buy-sell-loss" buySellLoss,
      testProperty "simple-sell" simpleSell,
      testProperty "sell-buy-profit" sellBuyProfit
    ]

simpleBuy :: Property
simpleBuy = property $ do
  b <- forAll $ genAnnotated genLot
  checkJournal
    ( do
        buy b
    )
    [ SawAction . Action . Buy <$> b,
      AddEvent . Opened True 1 <$> b,
      Result . Action . Buy <$> b,
      Result . Event . Open Long <$> b & details <>~ [Ident 1]
    ]

buyBuy :: Property
buyBuy = property $ do
  b <- forAll $ genAnnotated genLot
  checkJournal
    ( do
        buy b
        buy b
    )
    [ SawAction . Action . Buy <$> b,
      AddEvent . Opened True 1 <$> b,
      Result . Action . Buy <$> b,
      Result . Event . Open Long <$> b & details <>~ [Ident 1],
      SawAction . Action . Buy <$> b,
      AddEvent . Opened True 2 <$> b,
      Result . Action . Buy <$> b,
      Result . Event . Open Long <$> b & details <>~ [Ident 2]
    ]

buySellBreakeven :: Property
buySellBreakeven = property $ do
  b <- forAll $ genAnnotated genLot
  checkJournal
    ( do
        buy b
        sell b
    )
    [ SawAction . Action . Buy <$> b,
      AddEvent . Opened True 1 <$> b,
      Result . Action . Buy <$> b,
      Result . Event . Open Long <$> b & details <>~ [Ident 1],
      SawAction . Action . Sell <$> b,
      Result . Action . Sell <$> b,
      Result . Event . flip (Close Long) 0 <$> b & details <>~ [Ident 1],
      RemoveEvent 1 <$ b
    ]

buySellProfit :: Property
buySellProfit = property $ do
  b <- forAll $ genAnnotated genLot
  let sp = b & item . price +~ 10
  checkJournal
    ( do
        buy b
        sell sp
    )
    [ SawAction . Action . Buy <$> b,
      AddEvent . Opened True 1 <$> b,
      Result . Action . Buy <$> b,
      Result . Event . Open Long <$> b & details <>~ [Ident 1],
      SawAction . Action . Sell <$> sp,
      Result . Action . Sell <$> sp,
      Result . Event . flip (Close Long) 10 <$> b & details <>~ [Ident 1],
      RemoveEvent 1 <$ sp
    ]

buySellPartProfit :: Property
buySellPartProfit = property $ do
  b <- forAll $ genAnnotated genLot
  let b2 = b & item . amount *~ 2
      sp = b & item . price +~ 10
  checkJournal
    ( do
        buy b2
        sell sp
    )
    [ SawAction . Action . Buy <$> b2,
      AddEvent . Opened True 1 <$> b2,
      Result . Action . Buy <$> b2,
      Result . Event . Open Long <$> b2 & details <>~ [Ident 1],
      SawAction . Action . Sell <$> sp,
      Result . Action . Sell <$> sp,
      Result . Event . flip (Close Long) 10 <$> b & details <>~ [Ident 1],
      ReplaceEvent 1 . Opened True 1 <$> b
    ]

buySellLoss :: Property
buySellLoss = property $ do
  b <- forAll $ genAnnotated genLot
  let sl = b & item . price -~ 1
  checkJournal
    ( do
        buy b
        sell sl
    )
    [ SawAction . Action . Buy <$> b,
      AddEvent . Opened True 1 <$> b,
      Result . Action . Buy <$> b,
      Result . Event . Open Long <$> b & details <>~ [Ident 1],
      --
      SawAction . Action . Sell <$> sl,
      Result . Action . Sell <$> sl,
      Result . Event . flip (Close Long) (-1) <$> b & details <>~ [Ident 1],
      RemoveEvent 1 <$ sl,
      SubmitWash (sl ^. time) <$> (sl & item . price .~ 1),
      --
      SawAction . Event . Wash Future (sl ^. time) <$> (sl & item . price .~ 1),
      SubmitEnd <$ sl
    ]

buySellLossBuy :: Property
buySellLossBuy = property $ do
  b <- forAll $ genAnnotated genLot
  let sl =
        b & item . price -~ 1
          & details <>~ [WashTo "A" Nothing]
      b2 =
        b & details <>~ [WashApply "A" (b ^. item . amount)]
  checkJournal
    ( do
        buy b
        sell sl
        wash sl
        buy b2
    )
    [ SawAction . Action . Buy <$> b,
      AddEvent . Opened True 1 <$> b,
      Result . Action . Buy <$> b,
      Result . Event . Open Long <$> b & details <>~ [Ident 1],
      --
      SawAction . Action . Sell <$> sl,
      Result . Action . Sell <$> sl,
      Result . Event . flip (Close Long) (-1)
        <$> (b & details <>~ [WashTo "A" Nothing]) & details <>~ [Ident 1],
      RemoveEvent 1 <$ sl,
      SubmitWash (sl ^. time) <$> (sl & item . price .~ 1),
      --
      SawAction . Event . Wash Future (sl ^. time) <$> (sl & item . price .~ 1),
      SaveWash "A" <$> (sl & item . price .~ 1),
      Result . Event . Wash Future (sl ^. time) <$> (sl & item . price .~ 1),
      SubmitEnd <$ sl,
      --
      SawAction . Action . Buy <$> b2,
      AddEvent . Opened True 2 <$> b2 & details <>~ [Washed 1],
      Result . Action . Buy <$> b2,
      Result . Event . Open Long <$> b2 & details <>~ [Ident 2, Washed 1],
      Result . Event . Wash Present (sl ^. time) <$> (b2 & item . price .~ 1)
    ]

buyBuySellLoss :: Property
buyBuySellLoss = property $ do
  b <- forAll $ genAnnotated genLot
  let sl = b & item . price -~ 1
  checkJournal
    ( do
        buy b
        buy b
        sell sl
    )
    [ SawAction . Action . Buy <$> b,
      AddEvent . Opened True 1 <$> b,
      Result . Action . Buy <$> b,
      Result . Event . Open Long <$> b & details <>~ [Ident 1],
      --
      SawAction . Action . Buy <$> b,
      AddEvent . Opened True 2 <$> b,
      Result . Action . Buy <$> b,
      Result . Event . Open Long <$> b & details <>~ [Ident 2],
      --
      SawAction . Action . Sell <$> sl,
      Result . Action . Sell <$> sl,
      Result . Event . flip (Close Long) (-1) <$> b & details <>~ [Ident 1],
      RemoveEvent 1 <$ sl,
      SubmitWash (sl ^. time) <$> (sl & item . price .~ 1),
      --
      SawAction . Event . Wash Future (sl ^. time) <$> (sl & item . price .~ 1),
      RemoveEvent 2 <$ sl,
      AddEvent . Opened True 2 <$> b & details <>~ [Washed 1],
      Result
        . Event
        . Wash Past (sl ^. time)
        <$> ( b
                & item . price .~ 1
                & details <>~ [Ident 2]
            ),
      SubmitEnd <$ b
    ]

simpleSell :: Property
simpleSell = property $ do
  s <- forAll $ genAnnotated genLot
  checkJournal
    ( do
        sell s
    )
    [ SawAction . Action . Sell <$> s,
      AddEvent . Opened False 1 <$> s,
      Result . Action . Sell <$> s,
      Result . Event . Open Short <$> s & details <>~ [Ident 1]
    ]

sellBuyProfit :: Property
sellBuyProfit = property $ do
  b <- forAll $ genAnnotated genLot
  checkJournal
    ( do
        sell b
        buy b
    )
    [ SawAction . Action . Sell <$> b,
      AddEvent . Opened False 1 <$> b,
      Result . Action . Sell <$> b,
      Result . Event . Open Short <$> b & details <>~ [Ident 1],
      SawAction . Action . Buy <$> b,
      Result . Action . Buy <$> b,
      Result . Event . flip (Close Short) 0 <$> b & details <>~ [Ident 1],
      RemoveEvent 1 <$ b
    ]

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
-}

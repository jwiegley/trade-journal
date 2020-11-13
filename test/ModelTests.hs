{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module ModelTests (testModel) where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Int
import Data.Ratio
import Data.Time
import Data.Typeable
import Hedgehog hiding (Action)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Journal.Amount
import Journal.Model
import Journal.Types hiding (balance)
import Test.HUnit.Lang (FailureReason (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Text.Show.Pretty

{--------------------------------------------------------------------------}

data TestAction = TestAction
  { _action :: Timed Action,
    _annotations :: [Annotation],
    _regular :: Bool
  }

data TestState = TestState
  { _pending :: [TestAction],
    _balance :: Amount 2
  }

makeLenses ''TestState

buy :: Timed Lot -> [Annotation] -> State TestState ()
buy b anns =
  do
    bal <- use balance
    balance += c
    pending
      <>= [ TestAction
              (Buy <$> b)
              (anns ++ [Balance (bal + c), Net c])
              True
          ]
  where
    c = ((b ^. item . amount) * (b ^. item . price)) ^. coerced . to negate

sell :: Timed Lot -> [Annotation] -> State TestState ()
sell b anns =
  do
    bal <- use balance
    balance += c
    pending
      <>= [ TestAction
              (Sell <$> b)
              (anns ++ [Balance (bal + c), Net c])
              True
          ]
  where
    c = ((b ^. item . amount) * (b ^. item . price)) ^. coerced

wash :: Timed Lot -> [Annotation] -> State TestState ()
wash b anns =
  do
    bal <- use balance
    pending
      <>= [ TestAction
              (Wash <$> (b & item . price .~ 1))
              (anns ++ [Balance bal, Net 0])
              False
          ]

input :: State TestState a -> [Timed Action]
input =
  map _action
    . filter _regular
    . _pending
    . flip execState (TestState [] 0)

trades :: State TestState a -> [Timed Action]
trades =
  map (\x -> _action x & item . _Lot . details <>~ _annotations x)
    . _pending
    . flip execState (TestState [] 0)

checkJournal ::
  (MonadIO m, MonadTrans t) =>
  State TestState a ->
  [Change] ->
  t m ()
checkJournal journal expected =
  lift $
    processActionsWithChanges
      (input journal)
      @?== Right (expected, trades journal)

{--------------------------------------------------------------------------}

testModel :: TestTree
testModel =
  testGroup
    "model"
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
  b <- forAll $ genTimed genLot
  checkJournal
    ( do
        buy b [Position Open]
    )
    [ SawAction (Buy <$> b),
      AddEvent (Opened True <$> b),
      Result (Buy <$> (b & item . details <>~ [Position Open]))
    ]

buyBuy :: Property
buyBuy = property $ do
  b <- forAll $ genTimed genLot
  checkJournal
    ( do
        buy b [Position Open]
        buy b [Position Open]
    )
    [ SawAction (Buy <$> b),
      AddEvent (Opened True <$> b),
      Result (Buy <$> (b & item . details <>~ [Position Open])),
      SawAction (Buy <$> b),
      AddEvent (Opened True <$> b),
      Result (Buy <$> (b & item . details <>~ [Position Open]))
    ]

buySellBreakeven :: Property
buySellBreakeven = property $ do
  b <- forAll $ genTimed genLot
  checkJournal
    ( do
        buy b [Position Open]
        sell b [Position Close, Gain 0]
    )
    [ SawAction (Buy <$> b),
      AddEvent (Opened True <$> b),
      Result (Buy <$> (b & item . details <>~ [Position Open])),
      SawAction (Sell <$> b),
      Result (Sell <$> (b & item . details <>~ [Position Close, Gain 0])),
      RemoveEvent 1
    ]

buySellProfit :: Property
buySellProfit = property $ do
  b <- forAll $ genTimed genLot
  let sp = b & item . price +~ 10
  checkJournal
    ( do
        buy b [Position Open]
        sell sp [Position Close, Gain 10]
    )
    [ SawAction (Buy <$> b),
      AddEvent (Opened True <$> b),
      Result (Buy <$> (b & item . details <>~ [Position Open])),
      SawAction (Sell <$> sp),
      Result (Sell <$> (sp & item . details <>~ [Position Close, Gain 10])),
      RemoveEvent 1
    ]

buySellPartProfit :: Property
buySellPartProfit = property $ do
  b <- forAll $ genTimed genLot
  let b2 = b & item . amount *~ 2
      sp = b & item . price +~ 10
  checkJournal
    ( do
        buy b2 [Position Open]
        sell sp [Position Close, Gain 10]
    )
    [ SawAction (Buy <$> b2),
      AddEvent (Opened True <$> b2),
      Result (Buy <$> (b2 & item . details <>~ [Position Open])),
      SawAction (Sell <$> sp),
      Result (Sell <$> (sp & item . details <>~ [Position Close, Gain 10])),
      ReplaceEvent 1 (Opened True <$> b)
    ]

buySellLoss :: Property
buySellLoss = property $ do
  b <- forAll $ genTimed genLot
  let sl = b & item . price -~ 1
  checkJournal
    ( do
        buy b [Position Open]
        sell sl [Position Close, Loss 1]
    )
    [ SawAction (Buy <$> b),
      AddEvent (Opened True <$> b),
      Result (Buy <$> (b & item . details <>~ [Position Open])),
      SawAction (Sell <$> sl),
      Result (Sell <$> (sl & item . details <>~ [Position Close, Loss 1])),
      RemoveEvent 1,
      Submit (sl & item . price .~ 1),
      SubmitEnd
    ]

buySellLossBuy :: Property
buySellLossBuy = property $ do
  b <- forAll $ genTimed genLot
  let sl =
        b & item . price -~ 1
          & item . details <>~ [WashTo "A" Nothing]
      b2 =
        b & item . details <>~ [WashApply "A" (b ^. item . amount)]
  checkJournal
    ( do
        buy b [Position Open]
        sell sl [Position Close, Loss 1]
        wash sl []
        buy b2 [Position Open, Washed 1]
    )
    [ SawAction (Buy <$> b),
      AddEvent (Opened True <$> b),
      Result (Buy <$> (b & item . details <>~ [Position Open])),
      SawAction (Sell <$> sl),
      Result (Sell <$> (sl & item . details <>~ [Position Close, Loss 1])),
      RemoveEvent 1,
      Submit (sl & item . price .~ 1),
      SaveWash "A" (sl & item . price .~ 1),
      Result (Wash <$> (sl & item . price .~ 1)),
      SubmitEnd,
      SawAction (Buy <$> b2),
      AddEvent (Opened True <$> (b2 & item . details <>~ [Washed 1])),
      Result (Buy <$> (b2 & item . details <>~ [Position Open, Washed 1]))
    ]

buyBuySellLoss :: Property
buyBuySellLoss = property $ do
  b <- forAll $ genTimed genLot
  let sl = b & item . price -~ 1
  checkJournal
    ( do
        buy b [Position Open]
        buy b [Position Open]
        sell sl [Position Close, Loss 1]
        wash sl [Washed (b ^. item . price)]
    )
    [ SawAction (Buy <$> b),
      AddEvent (Opened True <$> b),
      Result (Buy <$> (b & item . details <>~ [Position Open])),
      SawAction (Buy <$> b),
      AddEvent (Opened True <$> b),
      Result (Buy <$> (b & item . details <>~ [Position Open])),
      SawAction (Sell <$> sl),
      Result (Sell <$> (sl & item . details <>~ [Position Close, Loss 1])),
      RemoveEvent 1,
      Submit (sl & item . price .~ 1),
      RemoveEvent 2,
      AddEvent (Opened True <$> (b & item . details <>~ [Washed 1])),
      Result
        ( Wash
            <$> ( b & item . price .~ 1
                    & item . details <>~ [Washed (b ^. item . price)]
                )
        ),
      SubmitEnd
    ]

simpleSell :: Property
simpleSell = property $ do
  s <- forAll $ genTimed genLot
  checkJournal
    ( do
        sell s [Position Open]
    )
    [ SawAction (Sell <$> s),
      AddEvent (Opened False <$> s),
      Result (Sell <$> (s & item . details <>~ [Position Open]))
    ]

sellBuyProfit :: Property
sellBuyProfit = property $ do
  b <- forAll $ genTimed genLot
  checkJournal
    ( do
        sell b [Position Open]
        buy b [Position Close, Gain 0]
    )
    [ SawAction (Sell <$> b),
      AddEvent (Opened False <$> b),
      Result (Sell <$> (b & item . details <>~ [Position Open])),
      SawAction (Buy <$> b),
      Result (Buy <$> (b & item . details <>~ [Position Close, Gain 0])),
      RemoveEvent 1
    ]

{--------------------------------------------------------------------------}

data MockFailure = MockFailure FailureReason
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

genAmount :: MonadGen m => Range Int64 -> m (Amount n)
genAmount range = do
  d <- Gen.integral range
  n <- Gen.integral range
  pure $ Amount (d % n)

genTimed :: MonadGen m => m a -> m (Timed a)
genTimed gen = do
  _time <- genUTCTime
  _item <- gen
  pure Timed {..}

genLot :: MonadGen m => m Lot
genLot = do
  q <- genAmount (Range.linear 1 1000)
  p <- genAmount (Range.linear 1 2000)
  pure $ Lot q "FOO" p [] []

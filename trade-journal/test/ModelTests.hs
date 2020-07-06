{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ModelTests (testModel) where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Int
import Data.Ratio
import Data.Time
import Data.Typeable
import Hedgehog hiding (Action)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Journal.Amount
import Journal.Model
import Journal.Types
import Test.HUnit.Lang (FailureReason (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Text.Show.Pretty

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

testModel :: TestTree
testModel =
  testGroup
    "model"
    [ baseline
    ]

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
  pure $ Lot q "FOO" p []

baseline :: TestTree
baseline =
  testGroup
    "baseline"
    [ testProperty "simple-buy" simpleBuy,
      testProperty "buy-buy" buyBuy,
      testProperty "buy-sell-breakeven" buySellBreakeven,
      testProperty "buy-sell-profit" buySellProfit,
      testProperty "buy-sell-part-profit" buySellPartProfit,
      testProperty "buy-buy-sell-loss" buyBuySellLoss,
      testProperty "buy-sell-loss-buy" buySellLossBuy,
      testProperty "buy-buy-sell-loss" buySellLoss,
      testProperty "simple-sell" simpleSell,
      testProperty "sell-buy-profit" sellBuyProfit
    ]

simpleBuy :: Property
simpleBuy = property $ do
  b <- forAll $ genTimed genLot
  -- A simple buy
  lift $
    processActionsWithChanges
      [Buy <$> b]
      @?== ( [ SawAction (Buy <$> b),
               AddEvent (Opened True <$> b),
               Result (Buy <$> (b & item . details <>~ [Position Open]))
             ],
             [Buy <$> (b & item . details <>~ [Position Open])]
           )

buyBuy :: Property
buyBuy = property $ do
  b <- forAll $ genTimed genLot
  lift $
    processActionsWithChanges
      [ Buy <$> b,
        Buy <$> b
      ]
      @?== ( [ SawAction (Buy <$> b),
               AddEvent (Opened True <$> b),
               Result (Buy <$> (b & item . details <>~ [Position Open])),
               SawAction (Buy <$> b),
               AddEvent (Opened True <$> b),
               Result (Buy <$> (b & item . details <>~ [Position Open]))
             ],
             [ Buy <$> (b & item . details <>~ [Position Open]),
               Buy <$> (b & item . details <>~ [Position Open])
             ]
           )

buySellBreakeven :: Property
buySellBreakeven = property $ do
  b <- forAll $ genTimed genLot
  -- A buy and sell at break-even
  lift $
    processActionsWithChanges
      [ Buy <$> b,
        Sell <$> b
      ]
      @?== ( [ SawAction (Buy <$> b),
               AddEvent (Opened True <$> b),
               Result (Buy <$> (b & item . details <>~ [Position Open])),
               SawAction (Sell <$> b),
               Result
                 ( Sell
                     <$> (b & item . details <>~ [Position Close, GainLoss 0])
                 ),
               RemoveEvent 1
             ],
             [ Buy <$> (b & item . details <>~ [Position Open]),
               Sell <$> (b & item . details <>~ [Position Close, GainLoss 0])
             ]
           )

buySellProfit :: Property
buySellProfit = property $ do
  b <- forAll $ genTimed genLot
  let s = b
      sp = s & item . price +~ 10
  -- A buy and sell at a profit
  lift $
    processActionsWithChanges
      [ Buy <$> b,
        Sell <$> sp
      ]
      @?== ( [ SawAction (Buy <$> b),
               AddEvent (Opened True <$> b),
               Result (Buy <$> (b & item . details <>~ [Position Open])),
               SawAction (Sell <$> sp),
               Result
                 ( Sell
                     <$> (sp & item . details <>~ [Position Close, GainLoss 10])
                 ),
               RemoveEvent 1
             ],
             [ Buy <$> (b & item . details <>~ [Position Open]),
               Sell <$> (sp & item . details <>~ [Position Close, GainLoss 10])
             ]
           )

buySellPartProfit :: Property
buySellPartProfit = property $ do
  b <- forAll $ genTimed genLot
  let b2 = b & item . amount *~ 2
      s = b
      sp = s & item . price +~ 10
  -- A buy and sell at a partial profit
  lift $
    processActionsWithChanges
      [ Buy <$> b2,
        Sell <$> sp
      ]
      @?== ( [ SawAction (Buy <$> b2),
               AddEvent (Opened True <$> b2),
               Result (Buy <$> (b2 & item . details <>~ [Position Open])),
               SawAction (Sell <$> sp),
               Result
                 ( Sell
                     <$> (sp & item . details <>~ [Position Close, GainLoss 10])
                 ),
               ReplaceEvent 1 (Opened True <$> b)
             ],
             [ Buy <$> (b2 & item . details <>~ [Position Open]),
               Sell <$> (sp & item . details <>~ [Position Close, GainLoss 10])
             ]
           )

buySellLoss :: Property
buySellLoss = property $ do
  b <- forAll $ genTimed genLot
  let s = b
      sl = s & item . price -~ 1
  -- A buy and sell at a loss
  lift $
    processActionsWithChanges
      [ Buy <$> b,
        Sell <$> sl
      ]
      @?== ( [ SawAction (Buy <$> b),
               AddEvent (Opened True <$> b),
               Result (Buy <$> (b & item . details <>~ [Position Open])),
               SawAction (Sell <$> sl),
               Result
                 ( Sell
                     <$> ( sl & item . details
                             <>~ [Position Close, GainLoss (-1)]
                         )
                 ),
               RemoveEvent 1,
               Submit (sl & item . price .~ 1),
               AddEvent (Adjustment <$> (sl & item . price .~ 1)),
               SubmitEnd
             ],
             [ Buy <$> (b & item . details <>~ [Position Open]),
               Sell
                 <$> (sl & item . details <>~ [Position Close, GainLoss (-1)])
             ]
           )

buySellLossBuy :: Property
buySellLossBuy = property $ do
  b <- forAll $ genTimed genLot
  let s = b
      sl = s & item . price -~ 1
  -- A buy and sell at a loss, followed by a buy
  lift $
    processActionsWithChanges
      [ Buy <$> b,
        Sell <$> sl,
        Buy <$> b
      ]
      @?== ( [ SawAction (Buy <$> b),
               AddEvent (Opened True <$> b),
               Result (Buy <$> (b & item . details <>~ [Position Open])),
               SawAction (Sell <$> sl),
               Result
                 ( Sell
                     <$> ( sl & item . details
                             <>~ [Position Close, GainLoss (-1)]
                         )
                 ),
               RemoveEvent 1,
               Submit (sl & item . price .~ 1),
               AddEvent (Adjustment <$> (sl & item . price .~ 1)),
               SubmitEnd,
               SawAction (Buy <$> b),
               RemoveEvent 2,
               AddEvent
                 ( Opened True
                     <$> ( b & item . details
                             <>~ [Washed OnOpen 1]
                         )
                 ),
               Result
                 ( Buy
                     <$> ( b & item . details
                             <>~ [ Position Open,
                                   Washed OnOpen 1
                                 ]
                         )
                 )
             ],
             [ Buy <$> (b & item . details <>~ [Position Open]),
               Sell
                 <$> (sl & item . details <>~ [Position Close, GainLoss (-1)]),
               Buy
                 <$> ( b & item . details
                         <>~ [ Position Open,
                               Washed OnOpen 1
                             ]
                     )
             ]
           )

buyBuySellLoss :: Property
buyBuySellLoss = property $ do
  b <- forAll $ genTimed genLot
  let s = b
      sl = s & item . price -~ 1
  -- A buy, a buy and then a sell at a loss
  lift $
    processActionsWithChanges
      [ Buy <$> b,
        Buy <$> b,
        Sell <$> sl
      ]
      @?== ( [ SawAction (Buy <$> b),
               AddEvent (Opened True <$> b),
               Result (Buy <$> (b & item . details <>~ [Position Open])),
               SawAction (Buy <$> b),
               AddEvent (Opened True <$> b),
               Result (Buy <$> (b & item . details <>~ [Position Open])),
               SawAction (Sell <$> sl),
               Result
                 ( Sell
                     <$> ( sl & item . details
                             <>~ [Position Close, GainLoss (-1)]
                         )
                 ),
               RemoveEvent 1,
               Submit (sl & item . price .~ 1),
               RemoveEvent 2,
               AddEvent
                 ( Opened True
                     <$> ( b & item . details
                             <>~ [Washed Retroactively 1]
                         )
                 ),
               SubmitEnd
             ],
             [ Buy <$> (b & item . details <>~ [Position Open]),
               Buy <$> (b & item . details <>~ [Position Open]),
               Sell
                 <$> (sl & item . details <>~ [Position Close, GainLoss (-1)])
             ]
           )

simpleSell :: Property
simpleSell = property $ do
  s <- forAll $ genTimed genLot
  -- A simple sell
  lift $
    processActionsWithChanges
      [Sell <$> s]
      @?== ( [ SawAction (Sell <$> s),
               AddEvent (Opened False <$> s),
               Result (Sell <$> (s & item . details <>~ [Position Open]))
             ],
             [Sell <$> (s & item . details <>~ [Position Open])]
           )

sellBuyProfit :: Property
sellBuyProfit = property $ do
  b <- forAll $ genTimed genLot
  let s = b
  -- A sell and buy at a profit
  lift $
    processActionsWithChanges
      [ Sell <$> s,
        Buy <$> b
      ]
      @?== ( [ SawAction (Sell <$> s),
               AddEvent (Opened False <$> s),
               Result (Sell <$> (s & item . details <>~ [Position Open])),
               SawAction (Buy <$> b),
               Result
                 ( Buy
                     <$> (b & item . details <>~ [Position Close, GainLoss 0])
                 ),
               RemoveEvent 1
             ],
             [ Sell <$> (s & item . details <>~ [Position Open]),
               Buy <$> (b & item . details <>~ [Position Close, GainLoss 0])
             ]
           )

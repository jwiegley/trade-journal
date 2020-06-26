{-# LANGUAGE OverloadedStrings #-}

module ModelTests where

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

genLot :: MonadGen m => m Lot
genLot = do
  q <- genAmount (Range.linear 1 1000)
  t <- genUTCTime
  pure $ Lot "FOO" q 1 t []

baseline :: TestTree
baseline =
  testGroup
    "baseline"
    [ testProperty "simple-buy" simpleBuy,
      testProperty "buy-buy" buyBuy,
      testProperty "buy-sell-breakeven" buySellBreakeven,
      testProperty "buy-sell-profit" buySellProfit,
      testProperty "buy-sell-part-profit" buySellPartProfit,
      testProperty "buy-sell-loss" buySellLoss,
      testProperty "buy-sell-loss-buy" buySellLossBuy,
      testProperty "buy-buy-sell-loss" buyBuySellLoss,
      testProperty "simple-sell" simpleSell,
      testProperty "sell-buy-profit" sellBuyProfit
    ]

simpleBuy :: Property
simpleBuy = property $ do
  b <- forAll genLot
  -- A simple buy
  lift $
    processActions
      [BuySell b]
      @?== ( [ Action (BuySell b),
               AddEntry (b & details <>~ [Position Open]),
               Result (b & details <>~ [Position Open])
             ],
             [b & details <>~ [Position Open]]
           )

buyBuy :: Property
buyBuy = property $ do
  b <- forAll genLot
  -- A simple buy
  lift $
    processActions
      [ BuySell b,
        BuySell b
      ]
      @?== ( [ Action (BuySell b),
               AddEntry (b & details <>~ [Position Open]),
               Result (b & details <>~ [Position Open]),
               Action (BuySell b),
               AddEntry (b & details <>~ [Position Open]),
               Result (b & details <>~ [Position Open])
             ],
             [ b & details <>~ [Position Open],
               b & details <>~ [Position Open]
             ]
           )

buySellBreakeven :: Property
buySellBreakeven = property $ do
  b <- forAll genLot
  let s = b & amount %~ negate
  -- A buy and sell at break-even
  lift $
    processActions
      [ BuySell b,
        BuySell s
      ]
      @?== ( [ Action (BuySell b),
               AddEntry (b & details <>~ [Position Open]),
               Result (b & details <>~ [Position Open]),
               Action (BuySell s),
               Result (s & details <>~ [GainLoss 0]),
               RemoveEntry 1
             ],
             [ b & details <>~ [Position Open],
               s & details <>~ [GainLoss 0]
             ]
           )

buySellProfit :: Property
buySellProfit = property $ do
  b <- forAll genLot
  let s = b & amount %~ negate
      sp = s & price +~ 10
  -- A buy and sell at a profit
  lift $
    processActions
      [ BuySell b,
        BuySell sp
      ]
      @?== ( [ Action (BuySell b),
               AddEntry (b & details <>~ [Position Open]),
               Result (b & details <>~ [Position Open]),
               Action (BuySell sp),
               Result (sp & details <>~ [GainLoss 10]),
               RemoveEntry 1
             ],
             [ b & details <>~ [Position Open],
               sp & details <>~ [GainLoss 10]
             ]
           )

buySellPartProfit :: Property
buySellPartProfit = property $ do
  b <- forAll genLot
  let b2 = b & amount *~ 2
      s = b & amount %~ negate
      sp = s & price +~ 10
  -- A buy and sell at a profit
  lift $
    processActions
      [ BuySell b2,
        BuySell sp
      ]
      @?== ( [ Action (BuySell b2),
               AddEntry (b2 & details <>~ [Position Open]),
               Result (b2 & details <>~ [Position Open]),
               Action (BuySell sp),
               Result (sp & details <>~ [GainLoss 10]),
               ReplaceEntry 1 (b & details <>~ [Position Open])
             ],
             [ b2 & details <>~ [Position Open],
               sp & details <>~ [GainLoss 10]
             ]
           )

buySellLoss :: Property
buySellLoss = property $ do
  b <- forAll genLot
  let s = b & amount %~ negate
      sl = s & price -~ 1
  -- A buy and sell at a loss
  lift $
    processActions
      [ BuySell b,
        BuySell sl
      ]
      @?== ( [ Action (BuySell b),
               AddEntry (b & details <>~ [Position Open]),
               Result (b & details <>~ [Position Open]),
               Action (BuySell sl),
               Result (sl & details <>~ [GainLoss (-1)]),
               RemoveEntry 1,
               Submit (Wash (sl & details <>~ [GainLoss (-1)])),
               AddEntry (sl & details .~ [WashSaleAdjust WashPending (-1)]),
               SubmitEnd
             ],
             [ b & details <>~ [Position Open],
               sl & details <>~ [GainLoss (-1)]
             ]
           )

buySellLossBuy :: Property
buySellLossBuy = property $ do
  b <- forAll genLot
  let s = b & amount %~ negate
      sl = s & price -~ 1
  -- A buy and sell at a loss
  lift $
    processActions
      [ BuySell b,
        BuySell sl,
        BuySell b
      ]
      @?== ( [ Action (BuySell b),
               AddEntry (b & details <>~ [Position Open]),
               Result (b & details <>~ [Position Open]),
               Action (BuySell sl),
               Result (sl & details <>~ [GainLoss (-1)]),
               Submit (Wash (sl & details <>~ [GainLoss (-1)])),
               AddEntry (sl & details .~ [WashSaleAdjust WashPending (-1)]),
               SubmitEnd,
               Action (BuySell b),
               AddEntry
                 ( b & details
                     <>~ [ Position Open,
                           WashSaleAdjust WashRetroactive (-1)
                         ]
                 ),
               Result
                 ( b & details
                     <>~ [ Position Open,
                           WashSaleAdjust WashRetroactive (-1)
                         ]
                 )
             ],
             [ b & details <>~ [Position Open],
               sl & details <>~ [GainLoss (-1)],
               b & details
                 <>~ [ Position Open,
                       WashSaleAdjust WashRetroactive (-1)
                     ]
             ]
           )

buyBuySellLoss :: Property
buyBuySellLoss = property $ do
  b <- forAll genLot
  let s = b & amount %~ negate
      sl = s & price -~ 1
  -- A buy and sell at a loss
  lift $
    processActions
      [ BuySell b,
        BuySell b,
        BuySell sl
      ]
      @?== ( [ Action (BuySell b),
               AddEntry (b & details <>~ [Position Open]),
               Result (b & details <>~ [Position Open]),
               Action (BuySell b),
               AddEntry (b & details <>~ [Position Open]),
               Result (b & details <>~ [Position Open]),
               Action (BuySell sl),
               Result (sl & details <>~ [GainLoss (-1)]),
               Submit (Wash (sl & details <>~ [GainLoss (-1)])),
               RemoveEntry 1,
               RemoveEntry 2,
               AddEntry
                 ( b & details
                     <>~ [ Position Open,
                           WashSaleAdjust WashRetroactive (-1)
                         ]
                 ),
               SubmitEnd
             ],
             [ b & details <>~ [Position Open],
               b & details <>~ [Position Open],
               sl & details <>~ [GainLoss (-1)]
             ]
           )

simpleSell :: Property
simpleSell = property $ do
  b <- forAll genLot
  let s = b & amount %~ negate
  -- A simple sell
  lift $
    processActions
      [BuySell s]
      @?== ( [ Action (BuySell s),
               AddEntry (s & details <>~ [Position Open]),
               Result (s & details <>~ [Position Open])
             ],
             [s & details <>~ [Position Open]]
           )

sellBuyProfit :: Property
sellBuyProfit = property $ do
  b <- forAll genLot
  let s = b & amount %~ negate
  -- A sell and buy at a profit
  lift $
    processActions
      [ BuySell s,
        BuySell b
      ]
      @?== ( [ Action (BuySell s),
               AddEntry (s & details <>~ [Position Open]),
               Result (s & details <>~ [Position Open]),
               Action (BuySell b),
               Result (b & details <>~ [GainLoss 0])
             ],
             [ s & details <>~ [Position Open],
               b & details <>~ [GainLoss 0]
             ]
           )

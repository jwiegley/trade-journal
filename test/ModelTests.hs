{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import Journal.Types
import Test.HUnit.Lang (FailureReason (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Text.Show.Pretty

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
  let c = (b ^. item . amount) * (b ^. item . price)
  -- A simple buy
  lift $
    processActionsWithChanges
      [Buy <$> b]
      @?== Right
        ( [ SawAction (Buy <$> b),
            AddEvent (Opened True <$> b),
            Result (Buy <$> (b & item . details <>~ [Position Open]))
          ],
          [ Buy
              <$> ( b & item . details
                      <>~ [ Position Open,
                            Balance (c ^. coerced . to negate),
                            Net (c ^. coerced . to negate)
                          ]
                  )
          ]
        )

buyBuy :: Property
buyBuy = property $ do
  b <- forAll $ genTimed genLot
  lift $
    processActionsWithChanges
      [ Buy <$> b,
        Buy <$> b
      ]
      @?== Right
        ( [ SawAction (Buy <$> b),
            AddEvent (Opened True <$> b),
            Result (Buy <$> (b & item . details <>~ [Position Open])),
            SawAction (Buy <$> b),
            AddEvent (Opened True <$> b),
            Result (Buy <$> (b & item . details <>~ [Position Open]))
          ],
          trades $ do
            buy b [Position Open]
            buy b [Position Open]
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
      @?== Right
        ( [ SawAction (Buy <$> b),
            AddEvent (Opened True <$> b),
            Result (Buy <$> (b & item . details <>~ [Position Open])),
            SawAction (Sell <$> b),
            Result
              ( Sell
                  <$> (b & item . details <>~ [Position Close, Gain 0])
              ),
            RemoveEvent 1
          ],
          trades $ do
            buy b [Position Open]
            sell b [Position Close, Gain 0]
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
      @?== Right
        ( [ SawAction (Buy <$> b),
            AddEvent (Opened True <$> b),
            Result (Buy <$> (b & item . details <>~ [Position Open])),
            SawAction (Sell <$> sp),
            Result
              ( Sell
                  <$> (sp & item . details <>~ [Position Close, Gain 10])
              ),
            RemoveEvent 1
          ],
          trades $ do
            buy b [Position Open]
            sell sp [Position Close, Gain 10]
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
      @?== Right
        ( [ SawAction (Buy <$> b2),
            AddEvent (Opened True <$> b2),
            Result (Buy <$> (b2 & item . details <>~ [Position Open])),
            SawAction (Sell <$> sp),
            Result
              ( Sell
                  <$> (sp & item . details <>~ [Position Close, Gain 10])
              ),
            ReplaceEvent 1 (Opened True <$> b)
          ],
          trades $ do
            buy b2 [Position Open]
            sell sp [Position Close, Gain 10]
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
      @?== Right
        ( [ SawAction (Buy <$> b),
            AddEvent (Opened True <$> b),
            Result (Buy <$> (b & item . details <>~ [Position Open])),
            SawAction (Sell <$> sl),
            Result
              ( Sell
                  <$> ( sl & item . details
                          <>~ [Position Close, Loss 1]
                      )
              ),
            RemoveEvent 1,
            Submit (sl & item . price .~ 1),
            SubmitEnd
          ],
          trades $ do
            buy b [Position Open]
            sell sl [Position Close, Loss 1]
        )

buySellLossBuy :: Property
buySellLossBuy = property $ do
  b <- forAll $ genTimed genLot
  let s = b
      sl =
        s & item . price -~ 1
          & item . details <>~ [WashTo "A" Nothing]
      b2 =
        b & item . details <>~ [WashApply "A" (b ^. item . amount)]
  -- A buy and sell at a loss, followed by a buy
  lift $
    processActionsWithChanges
      [ Buy <$> b,
        Sell <$> sl,
        Buy <$> b2
      ]
      @?== Right
        ( [ SawAction (Buy <$> b),
            AddEvent (Opened True <$> b),
            Result (Buy <$> (b & item . details <>~ [Position Open])),
            SawAction (Sell <$> sl),
            Result
              ( Sell
                  <$> ( sl & item . details
                          <>~ [Position Close, Loss 1]
                      )
              ),
            RemoveEvent 1,
            Submit (sl & item . price .~ 1),
            SaveWash "A" (sl & item . price .~ 1),
            Result (Wash <$> (sl & item . price .~ 1)),
            SubmitEnd,
            SawAction (Buy <$> b2),
            AddEvent
              ( Opened True
                  <$> ( b2 & item . details
                          <>~ [Washed 1]
                      )
              ),
            Result
              ( Buy
                  <$> ( b2 & item . details
                          <>~ [ Position Open,
                                Washed 1
                              ]
                      )
              )
          ],
          trades $ do
            buy b [Position Open]
            sell sl [Position Close, Loss 1]
            wash sl []
            buy b2 [Position Open, Washed 1]
        )

buyBuySellLoss :: Property
buyBuySellLoss = property $ do
  b <- forAll $ genTimed genLot
  let sl = b & item . price -~ 1
  -- A buy, a buy and then a sell at a loss
  lift $
    processActionsWithChanges
      [ Buy <$> b,
        Buy <$> b,
        Sell <$> sl
      ]
      @?== Right
        ( [ SawAction (Buy <$> b),
            AddEvent (Opened True <$> b),
            Result (Buy <$> (b & item . details <>~ [Position Open])),
            SawAction (Buy <$> b),
            AddEvent (Opened True <$> b),
            Result (Buy <$> (b & item . details <>~ [Position Open])),
            SawAction (Sell <$> sl),
            Result
              ( Sell
                  <$> ( sl & item . details
                          <>~ [Position Close, Loss 1]
                      )
              ),
            RemoveEvent 1,
            Submit (sl & item . price .~ 1),
            RemoveEvent 2,
            AddEvent
              ( Opened True
                  <$> ( b & item . details
                          <>~ [Washed 1]
                      )
              ),
            Result
              ( Wash
                  <$> ( b & item . price .~ 1
                          & item . details <>~ [Washed (b ^. item . price)]
                      )
              ),
            SubmitEnd
          ],
          trades $ do
            buy b [Position Open]
            buy b [Position Open]
            sell sl [Position Close, Loss 1]
            wash sl [Washed (b ^. item . price)]
        )

simpleSell :: Property
simpleSell = property $ do
  s <- forAll $ genTimed genLot
  let c = (s ^. item . amount) * (s ^. item . price)
  -- A simple sell
  lift $
    processActionsWithChanges
      [Sell <$> s]
      @?== Right
        ( [ SawAction (Sell <$> s),
            AddEvent (Opened False <$> s),
            Result (Sell <$> (s & item . details <>~ [Position Open]))
          ],
          [ Sell
              <$> ( s & item . details
                      <>~ [ Position Open,
                            Balance (c ^. coerced),
                            Net (c ^. coerced)
                          ]
                  )
          ]
        )

sellBuyProfit :: Property
sellBuyProfit = property $ do
  b <- forAll $ genTimed genLot
  -- A sell and buy at a profit
  lift $
    processActionsWithChanges
      [ Sell <$> b,
        Buy <$> b
      ]
      @?== Right
        ( [ SawAction (Sell <$> b),
            AddEvent (Opened False <$> b),
            Result (Sell <$> (b & item . details <>~ [Position Open])),
            SawAction (Buy <$> b),
            Result
              ( Buy
                  <$> (b & item . details <>~ [Position Close, Gain 0])
              ),
            RemoveEvent 1
          ],
          trades $ do
            sell b [Position Open]
            buy b [Position Close, Gain 0]
        )

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

{--------------------------------------------------------------------------}

buy :: Timed Lot -> [Annotation] -> State ([Timed Action], Amount 2) ()
buy b anns =
  do
    bal <- use _2
    _2 += c
    _1
      <>= [ Buy
              <$> ( b & item . details
                      <>~ (anns ++ [Balance (bal + c), Net c])
                  )
          ]
  where
    c = ((b ^. item . amount) * (b ^. item . price)) ^. coerced . to negate

sell :: Timed Lot -> [Annotation] -> State ([Timed Action], Amount 2) ()
sell b anns =
  do
    bal <- use _2
    _2 += c
    _1
      <>= [ Sell
              <$> ( b & item . details
                      <>~ (anns ++ [Balance (bal + c), Net c])
                  )
          ]
  where
    c = ((b ^. item . amount) * (b ^. item . price)) ^. coerced

wash :: Timed Lot -> [Annotation] -> State ([Timed Action], Amount 2) ()
wash b anns =
  do
    bal <- use _2
    _1
      <>= [ Wash
              <$> ( b & item . price .~ 1
                      & item . details
                      <>~ (anns ++ [Balance bal, Net 0])
                  )
          ]

trades :: State ([Timed Action], Amount 2) a -> [Timed Action]
trades = fst . flip execState ([], 0)

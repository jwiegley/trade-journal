{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module WashSaleRule where

import Amount
import Control.Arrow
import Control.Lens
import Control.Monad.State
import Hedgehog hiding (Action)
import qualified Hedgehog.Gen as Gen
import Journal.Closings
import Journal.Entry.Deposit
import Journal.Entry.Income
import Journal.Entry.Options
import Journal.Entry.Trade
import Journal.Types
import Taxes.USA.WashSaleRule
import Test.Tasty
import Test.Tasty.Hedgehog
import TestAction hiding (positions)

testWashSaleRule :: TestTree
testWashSaleRule =
  testGroup
    "wash-sale-rule"
    [ testRule "buy-buy-sell" $ \runTest b -> do
        let s = b & item . price -~ 10
        runTest
          do
            buy b
            buy b
            buy b
            sell s
          do
            buy b
            open 1 Long b
            --
            buy b
            open 2 Long b
            --
            buy b
            washedFrom Future s (-10) $
              open 3 Long b
            --
            sell s
            wash Past 3 (-10) $
              close 1 s
          do
            open 2 Long b
            washedFrom Future s (-10) $
              open 3 Long b,
      --
      testRule "buy-sell-buy" $ \runTest b -> do
        let s = b & item . price -~ 10
        runTest
          do
            buy b
            sell s
            buy b
            buy b
          do
            buy b
            open 1 Long b
            --
            sell s
            wash Future 2 (-10) $
              close 1 s
            --
            buy b
            washedFrom Past s (-10) $
              open 2 Long b
            --
            buy b
            open 3 Long b
          do
            washedFrom Past s (-10) $
              open 2 Long b
            open 3 Long b,
      --
      testRule "buy-buy-sell-buy-sell" $ \runTest b -> do
        let s = b & item . price -~ 10
        runTest
          do
            buy b
            buy b
            sell b
            buy b
            sell s
          do
            buy b
            open 1 Long b
            --
            buy b
            open 2 Long b
            --
            sell b
            close 1 b
            --
            buy b
            washedFrom Future s (-10) $
              open 3 Long b
            --
            sell s
            wash Past 3 (-10) $
              close 2 s
          do
            washedFrom Future s (-10) $
              open 3 Long b,
      --
      testRule "buy-buy2-sell-sell" $ \runTest b -> do
        let b2 = b & item . amount *~ 2
        let s = b & item . price -~ 10
        runTest
          do
            buy b
            buy b2
            sell s
            sell b2
          do
            buy b
            open 1 Long b
            --
            buy b2
            washedFrom Future s (-10) $
              open 2 Long b2
            --
            sell s
            wash Past 2 (-10) $
              close 1 s
            --
            sell b2
            close 2 b2
          do
            pure (),
      --
      testRule "buy-buy-buy-sell2-buy" $ \runTest b -> do
        let s = b & item . price -~ 10
            s2 = s & item . amount *~ 2
        runTest
          do
            buy b
            buy b
            buy b
            sell s2
            buy b
          do
            buy b
            open 1 Long b
            --
            buy b
            open 2 Long b
            --
            buy b
            washedFrom Future s (-10) $
              open 3 Long b
            --
            sell s2
            wash Past 3 (-10) $
              close 1 s
            wash Future 4 (-10) $
              close 2 s
            --
            buy b
            washedFrom Past s (-10) $
              open 4 Long b
          do
            washedFrom Future s (-10) $
              open 3 Long b
            washedFrom Past s (-10) $
              open 4 Long b,
      --
      testRule "buy3-sell2-sell-buy" $ \runTest b -> do
        let b3 = b & item . amount *~ 3
        let s2 =
              b & item . amount *~ 2
                & item . price -~ 10
        runTest
          do
            buy b3
            sell s2
            sell b
            buy b
          do
            buy b3
            open 1 Long b3
            --
            sell s2
            wash Future 2 (-10) $
              close 1 s2
            --
            sell b
            close 1 b
            --
            buy b
            washedFrom Past s2 (-10) $
              open 2 Long b
          do
            washedFrom Past s2 (-10) $
              open 2 Long b,
      --
      testRule "buy-buy-sell-sell" $ \runTest b -> do
        let s = b & item . price -~ 10
        runTest
          do
            buy b
            buy b
            sell s
            sell s
          do
            buy b
            open 1 Long b
            --
            buy b
            washedFrom Future s (-10) $
              open 2 Long b
            --
            sell s
            wash Past 2 (-10) $
              close 1 s
            --
            sell s
            close 2 s
          -- jww (2021-12-07): Implement a balance checker:
          -- balance AMOUNT
          do
            pure ()
    ]

testRule ::
  String ->
  ( ( TestDSL '[Const Trade, Const Deposit, Const Income, Const Options] () ->
      TestDSL
        '[ Const Washing,
           Const PositionEvent,
           Const Trade,
           Const Deposit,
           Const Income,
           Const Options
         ]
        () ->
      TestDSL
        '[ Const Washing,
           Const PositionEvent,
           Const Trade,
           Const Deposit,
           Const Income,
           Const Options
         ]
        () ->
      PropertyT IO ()
    ) ->
    Annotated Lot ->
    PropertyT IO ()
  ) ->
  TestTree
testRule name f = testProperty name $
  property $ do
    b <-
      forAll $
        Gen.filter (\b -> (b ^. item . price) > 10) $
          genAnnotated genLot
    f (checkJournal washSaleRuleTest) b
  where
    washSaleRuleTest = (id &&& positions) . washSaleRule . fst

wash ::
  Period ->
  Int ->
  Amount 6 ->
  TestDSL
    '[ Const PositionEvent,
       Const Trade,
       Const Deposit,
       Const Income,
       Const Options
     ]
    () ->
  TestDSL
    '[ Const Washing,
       Const PositionEvent,
       Const Trade,
       Const Deposit,
       Const Income,
       Const Options
     ]
    ()
wash
  period
  n
  pl
  (flip execState [] -> [APositionEvent c@(view item -> Close cl)]) =
    id
      <>= [ AWashing
              ( Wash
                  { _washPeriod = period,
                    _washPositionIdent = n,
                    _washCostBasis =
                      cl ^. closingLot . price - pl,
                    _washClosing = cl
                  }
                  <$ c
              )
          ]
wash _ _ _ _ = error "Incorrect use of wash"

washedFrom ::
  Period ->
  Annotated Lot ->
  Amount 6 ->
  TestDSL
    '[ Const PositionEvent,
       Const Trade,
       Const Deposit,
       Const Income,
       Const Options
     ]
    () ->
  TestDSL
    '[ Const Washing,
       Const PositionEvent,
       Const Trade,
       Const Deposit,
       Const Income,
       Const Options
     ]
    ()
washedFrom
  period
  lot
  loss
  (flip execState [] -> [APositionEvent o@(view item -> Open pos)]) =
    id
      <>= [ AWashing
              ( let proratedLoss =
                      ( (loss * lot ^. item . amount)
                          / (pos ^. posLot . amount)
                      )
                 in WashedFrom
                      { _washedFromPeriod = period,
                        _washedFromClosingLot = lot ^. item,
                        _washedFromCostBasis = pos ^. posLot . price,
                        _washedPosition = pos & posLot . price -~ proratedLoss
                      }
                      <$ o
              )
          ]
washedFrom _ _ _ _ = error "Incorrect use of washedFrom"

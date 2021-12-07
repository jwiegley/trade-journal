{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module WashSaleRule where

import Amount
import Control.Arrow
import Control.Lens
import Control.Monad.State
import Data.Sum
import Hedgehog hiding (Action)
import qualified Hedgehog.Gen as Gen
import Journal.Closings
import Journal.SumLens
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
            wash Past 3 $
              close 1 s (-10)
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
            wash Future 2 $
              close 1 s (-10)
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
            close 1 b 0
            --
            buy b
            washedFrom Future s (-10) $
              open 3 Long b
            --
            sell s
            wash Past 3 $
              close 2 s (-10)
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
            wash Past 2 $
              close 1 s (-10)
            --
            sell b2
            close 2 b2 (-10 / 2)
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
            wash Past 3 $
              close 1 s (-10)
            wash Future 4 $
              close 2 s (-10)
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
            wash Future 2 $
              close 1 s2 (-10)
            --
            sell b
            close 1 b 0
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
            wash Past 2 $
              close 1 s (-10)
            --
            sell s
            close 2 s (-20)
          do
            pure ()
    ]

testRule ::
  String ->
  ( ( TestDSL '[Const Entry] () ->
      TestDSL '[Const Washing, Const PositionEvent, Const Entry] () ->
      TestDSL '[Const Washing, Const PositionEvent, Const Entry] () ->
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
  TestDSL '[Const PositionEvent, Const Entry] () ->
  TestDSL '[Const Washing, Const PositionEvent, Const Entry] ()
wash period n (flip execState [] -> [c]) = undefined
-- id
--   <>= [ c & projectedC . _EClose %~ \cl ->
--           cl
--             & eCloseData
--               <>~ [ Wash
--                       { _washPeriod = period,
--                         _washPositionIdent = n,
--                         _washCostBasis =
--                           cl ^. eCloseLot . item . price
--                             - cl ^. eClosePL
--                       }
--                   ]
--       ]
wash _ _ _ = error "Incorrect use of wash"

washedFrom ::
  Period ->
  Annotated Lot ->
  Amount 6 ->
  TestDSL '[Const PositionEvent, Const Entry] () ->
  TestDSL '[Const Washing, Const PositionEvent, Const Entry] ()
washedFrom period lot loss (flip execState [] -> [o]) = undefined
-- id
--   <>= [ o & _EOpen . item %~ \pos ->
--           let proratedLoss =
--                 ( (loss * lot ^. item . amount)
--                     / (pos ^. posLot . amount)
--                 )
--            in pos
--                 & posLot . price -~ proratedLoss
--                 & posData
--                   <>~ [ WashedFrom
--                           { _washedFromPeriod = period,
--                             _washedFromClosingLot = lot ^. item,
--                             _washedFromCostBasis = pos ^. posLot . price
--                           }
--                       ]
--       ]
washedFrom _ _ _ _ = error "Incorrect use of washedFrom"

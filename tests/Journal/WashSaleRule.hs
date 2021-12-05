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
import Journal.Types
import Taxes.USA.WashSaleRule
import Test.Tasty
import Test.Tasty.Hedgehog
import TestAction hiding (positions)

testWashSaleRule :: TestTree
testWashSaleRule =
  testGroup
    "wash-sale-rule"
    [ testRule "buy-buy-sell" $ \runTest b ->
        runTest
          do
            buy b
            buy b
            buy b
            sell $ b & item . price -~ 10
          do
            buy b
            open 1 Long b
            --
            buy b
            open 2 Long b
            --
            buy b
            washedFrom Future (-10) $
              open 3 Long b
            --
            sell $ b & item . price -~ 10
            wash Past 3 $
              close 1 b (-10)
          do
            open 2 Long b
            washedFrom Future (-10) $
              open 3 Long b,
      --
      testRule "buy-sell-buy" $ \runTest b ->
        runTest
          do
            buy b
            sell $ b & item . price -~ 10
            buy b
            buy b
          do
            buy b
            open 1 Long b
            --
            sell $ b & item . price -~ 10
            wash Future 2 $
              close 1 b (-10)
            --
            buy b
            washedFrom Past (-10) $
              open 2 Long b
            --
            buy b
            open 3 Long b
          do
            washedFrom Past (-10) $
              open 2 Long b
            open 3 Long b,
      --
      testRule "buy-buy-sell-buy-sell" $ \runTest b ->
        runTest
          do
            buy b
            buy b
            sell b
            buy b
            sell $ b & item . price -~ 10
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
            washedFrom Future (-10) $
              open 3 Long b
            --
            sell $ b & item . price -~ 10
            wash Past 3 $
              close 2 b (-10)
          do
            washedFrom Future (-10) $
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
            open 2 Long b2
            --
            sell b
            close 1 b 0
            --
            sell b2
            close 2 b2 0
          do
            pure (),
      --
      testRule "buy-buy-buy-sell2-buy" $ \runTest b -> do
        let s2 =
              b & item . price -~ 10
                & item . amount *~ 2
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
            open 3 Long b
            --
            sell s2
            close 1 b 0
            close 2 b 0
            --
            buy b
            open 4 Long b
          do
            pure (),
      --
      testRule "buy3-sell2-sell-buy" $ \runTest b -> do
        let b3 = b & item . amount *~ 2
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
            close 1 s2 0
            --
            sell b
            close 1 b 0
            --
            buy b
            open 2 Long b
          do
            pure (),
      --
      testRule "buy-buy-sell-sell" $ \runTest b ->
        runTest
          do
            buy b
            buy b
            let s = b & item . price -~ 10
            sell s
            sell s
          do
            buy b
            open 1 Long b
            --
            buy b
            washedFrom Future (-10) $
              open 2 Long b
            --
            let s = b & item . price -~ 10
            -- sell s
            -- wash Past 2 $
            --   close 1 s 0
            sell s
            wash Past 2 $
              close 1 b (-10)
            --
            sell s
            close 2 s (-20)
          do
            pure mempty
    ]

testRule ::
  String ->
  ( ( TestDSL [Washing] () ->
      TestDSL [Washing] () ->
      TestDSL [Washing] () ->
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

washedFrom ::
  Period ->
  Amount 6 ->
  TestDSL [Washing] () ->
  TestDSL [Washing] ()
washedFrom period loss (flip execState [] -> [o]) =
  id
    <>= [ o & _EOpen . item %~ \pos ->
            pos
              & posBasis -~ loss
              & posData
                <>~ [ WashedFrom
                        period
                        loss
                        (pos ^. posLot & price +~ loss)
                    ]
        ]
washedFrom _ _ _ = error "Incorrect use of washedFrom"

wash ::
  Period ->
  Int ->
  TestDSL [Washing] () ->
  TestDSL [Washing] ()
wash period n (flip execState [] -> [c]) =
  id
    <>= [ c & _EClose %~ \cl ->
            cl
              & _2 . item . price +~ (cl ^. _3)
              & _4 <>~ [Wash period (cl ^. _3) n]
        ]
wash _ _ _ = error "Incorrect use of wash"

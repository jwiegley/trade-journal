{-# LANGUAGE BlockArguments #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE ViewPatterns #-}

module TestWashSaleRule2 where

-- import Amount
-- import Control.Lens hiding (Context)

import Data.Map qualified as M
import Data.Time
-- import Data.Time.Format
-- import Hedgehog hiding (Action)
-- import Hedgehog.Gen qualified as Gen
import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.Hedgehog
import Trade.Taxes.USA.WashSaleRule2

testWashSaleRule2 :: TestTree
testWashSaleRule2 =
  testGroup
    "wash-sale-rule"
    [ testAddLot,
      testAddToLots,
      testIdentifyTrades,
      testWashSales
    ]

sometime :: DiffTime -> UTCTime
sometime = UTCTime (ModifiedJulianDay 0)

now50 :: TimePrice
now50 = TimePrice 50 (UTCTime (ModifiedJulianDay 0) 0)

now100 :: TimePrice
now100 = TimePrice 100 (UTCTime (ModifiedJulianDay 0) 0)

now200 :: TimePrice
now200 = TimePrice 200 (UTCTime (ModifiedJulianDay 0) 0)

testAddLot :: TestTree
testAddLot =
  testGroup
    "addLot"
    [ testCase "add-lot-pos-pos=-price==" do
        addLot (Lot 10 now100) (Lot 10 now100)
          @?= Just (AddLot (Lot 20 now100)),
      testCase "add-lot-pos-pos=-price<" do
        addLot (Lot 10 now100) (Lot 10 now50)
          @?= Nothing,
      testCase "add-lot-pos-pos=-price>" do
        addLot (Lot 10 now100) (Lot 10 now200)
          @?= Nothing,
      testCase "add-lot-neg-neg=-price==" do
        addLot (Lot (-10) now100) (Lot (-10) now100)
          @?= Just (AddLot (Lot (-20) now100)),
      testCase "add-lot-neg-neg=-price<" do
        addLot (Lot (-10) now100) (Lot (-10) now50)
          @?= Nothing,
      testCase "add-lot-neg-neg=-price>" do
        addLot (Lot (-10) now100) (Lot (-10) now200)
          @?= Nothing,
      testCase "add-lot-pos-neg=-price==" do
        addLot (Lot 10 now100) (Lot (-10) now100)
          @?= Just (ReduceLot (Left (Lot 0 now100))),
      testCase "add-lot-pos-neg=-price<" do
        addLot (Lot 10 now100) (Lot (-10) now50)
          @?= Just (ReduceLot (Left (Lot 0 now100))),
      testCase "add-lot-pos-neg=-price>" do
        addLot (Lot 10 now100) (Lot (-10) now200)
          @?= Just (ReduceLot (Left (Lot 0 now100))),
      testCase "add-lot-pos-neg<-price==" do
        addLot (Lot 10 now100) (Lot (-5) now100)
          @?= Just (ReduceLot (Left (Lot 5 now100))),
      testCase "add-lot-pos-neg<-price<" do
        addLot (Lot 10 now100) (Lot (-5) now50)
          @?= Just (ReduceLot (Left (Lot 5 now100))),
      testCase "add-lot-pos-neg<-price>" do
        addLot (Lot 10 now100) (Lot (-5) now200)
          @?= Just (ReduceLot (Left (Lot 5 now100))),
      testCase "add-lot-pos-neg>-price==" do
        addLot (Lot 10 now100) (Lot (-20) now100)
          @?= Just (ReduceLot (Right (Lot (-10) now100))),
      testCase "add-lot-pos-neg>-price<" do
        addLot (Lot 10 now100) (Lot (-20) now50)
          @?= Just (ReduceLot (Right (Lot (-10) now50))),
      testCase "add-lot-pos-neg>-price>" do
        addLot (Lot 10 now100) (Lot (-20) now200)
          @?= Just (ReduceLot (Right (Lot (-10) now200))),
      testCase "add-lot-neg-pos=-price==" do
        addLot (Lot (-10) now100) (Lot 10 now100)
          @?= Just (ReduceLot (Left (Lot 0 now100))),
      testCase "add-lot-neg-pos=-price<" do
        addLot (Lot (-10) now100) (Lot 10 now50)
          @?= Just (ReduceLot (Left (Lot 0 now100))),
      testCase "add-lot-neg-pos=-price>" do
        addLot (Lot (-10) now100) (Lot 10 now200)
          @?= Just (ReduceLot (Left (Lot 0 now100))),
      testCase "add-lot-neg-pos<-price==" do
        addLot (Lot (-10) now100) (Lot 5 now100)
          @?= Just (ReduceLot (Left (Lot (-5) now100))),
      testCase "add-lot-neg-pos<-price<" do
        addLot (Lot (-10) now100) (Lot 5 now50)
          @?= Just (ReduceLot (Left (Lot (-5) now100))),
      testCase "add-lot-neg-pos<-price>" do
        addLot (Lot (-10) now100) (Lot 5 now200)
          @?= Just (ReduceLot (Left (Lot (-5) now100))),
      testCase "add-lot-neg-pos>-price==" do
        addLot (Lot (-10) now100) (Lot 20 now100)
          @?= Just (ReduceLot (Right (Lot 10 now100))),
      testCase "add-lot-neg-pos>-price<" do
        addLot (Lot (-10) now100) (Lot 20 now50)
          @?= Just (ReduceLot (Right (Lot 10 now50))),
      testCase "add-lot-neg-pos>-price>" do
        addLot (Lot (-10) now100) (Lot 20 now200)
          @?= Just (ReduceLot (Right (Lot 10 now200)))
    ]

testAddToLots :: TestTree
testAddToLots =
  testGroup
    "addToLots"
    [ testCase "add-to-lots-pos-pos=-price==" do
        addToPositions
          id
          (Lot 10 now100)
          [Open (Lot 10 now100) Nothing]
          @?= [Open (Lot 20 now100) Nothing],
      testCase "add-to-lots-pos-many" do
        addToPositions
          id
          (Lot (-100) now200)
          [ Open (Lot 10 now100) Nothing,
            Open (Lot 20 now100) Nothing
          ]
          @?= [ Closed (Lot 10 now100) now200 True,
                Closed (Lot 20 now100) now200 True,
                Open (Lot (-70) now200) Nothing
              ],
      testCase "add-to-lots-pos-few-fifo" do
        addToPositions
          id
          (Lot (-5) now200)
          [ Open (Lot 10 now100) Nothing,
            Open (Lot 20 now100) Nothing
          ]
          @?= [ Open (Lot 5 now100) Nothing,
                Closed (Lot 5 now100) now200 True,
                Open (Lot 20 now100) Nothing
              ],
      testCase "add-to-lots-pos-few-lifo" do
        addToPositions
          reverse
          (Lot (-5) now200)
          [ Open (Lot 10 now100) Nothing,
            Open (Lot 20 now100) Nothing
          ]
          @?= [ Open (Lot 10 now100) Nothing,
                Closed (Lot 5 now100) now200 True,
                Open (Lot 15 now100) Nothing
              ]
    ]

testIdentifyTrades :: TestTree
testIdentifyTrades =
  testGroup
    "identifyTrades"
    [ testCase "identify-trades-smoke" do
        identifyTrades id mempty [("AAPL", Lot 10 now100)]
          @?= M.fromList [("AAPL", [Open (Lot 10 now100) Nothing])]
    ]

testWashSales :: TestTree
testWashSales =
  testGroup
    "washSales"
    [ testCase "wash-sales-noop" do
        washSales [Open (Lot 10 (TimePrice 100 (sometime 0))) Nothing]
          @?= [Open (Lot 10 (TimePrice 100 (sometime 0))) Nothing],
      testCase "wash-sales-buy-sell-buy" do
        washSales
          [ open 10 100 0 Nothing,
            closed 10 100 0 50 10 True,
            open 10 70 20 Nothing
          ]
          @?= [ open 10 100 0 Nothing,
                closed 10 100 0 50 10 False,
                open 10 70 20 (Just (70 + 50))
              ],
      testCase "wash-sales-buy-buy-sell" do
        washSales
          [ open 10 100 0 Nothing,
            open 10 70 10 Nothing,
            closed 5 100 0 50 20 True
          ]
          @?= [ open 10 100 0 Nothing,
                open 10 70 10 (Just (70 + (5 * (100 - 50)) / 10)),
                closed 5 100 0 50 20 False
              ]
    ]
  where
    open n p d =
      Open (Lot n (TimePrice p (sometime d)))
    closed n p d p' d' =
      Closed
        (Lot n (TimePrice p (sometime d)))
        (TimePrice p' (sometime d'))

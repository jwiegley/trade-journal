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
      testIdentifyTrades
    ]

now50 :: TimePrice
now50 = TimePrice 50.0 (UTCTime (ModifiedJulianDay 0) 0)

now100 :: TimePrice
now100 = TimePrice 100.0 (UTCTime (ModifiedJulianDay 0) 0)

now200 :: TimePrice
now200 = TimePrice 200.0 (UTCTime (ModifiedJulianDay 0) 0)

testAddLot :: TestTree
testAddLot =
  testGroup
    "addLot"
    [ testCase "add-lot-pos-pos=-price==" do
        addLot (Lot 10.0 now100) (Lot 10.0 now100)
          @?= Just (AddLot (Lot 20.0 now100)),
      testCase "add-lot-pos-pos=-price<" do
        addLot (Lot 10.0 now100) (Lot 10.0 now50)
          @?= Nothing,
      testCase "add-lot-pos-pos=-price>" do
        addLot (Lot 10.0 now100) (Lot 10.0 now200)
          @?= Nothing,
      testCase "add-lot-neg-neg=-price==" do
        addLot (Lot (-10.0) now100) (Lot (-10.0) now100)
          @?= Just (AddLot (Lot (-20.0) now100)),
      testCase "add-lot-neg-neg=-price<" do
        addLot (Lot (-10.0) now100) (Lot (-10.0) now50)
          @?= Nothing,
      testCase "add-lot-neg-neg=-price>" do
        addLot (Lot (-10.0) now100) (Lot (-10.0) now200)
          @?= Nothing,
      testCase "add-lot-pos-neg=-price==" do
        addLot (Lot 10.0 now100) (Lot (-10.0) now100)
          @?= Just (ReduceLot (Left (Lot 0 now100))),
      testCase "add-lot-pos-neg=-price<" do
        addLot (Lot 10.0 now100) (Lot (-10.0) now50)
          @?= Just (ReduceLot (Left (Lot 0 now100))),
      testCase "add-lot-pos-neg=-price>" do
        addLot (Lot 10.0 now100) (Lot (-10.0) now200)
          @?= Just (ReduceLot (Left (Lot 0 now100))),
      testCase "add-lot-pos-neg<-price==" do
        addLot (Lot 10.0 now100) (Lot (-5.0) now100)
          @?= Just (ReduceLot (Left (Lot 5.0 now100))),
      testCase "add-lot-pos-neg<-price<" do
        addLot (Lot 10.0 now100) (Lot (-5.0) now50)
          @?= Just (ReduceLot (Left (Lot 5.0 now100))),
      testCase "add-lot-pos-neg<-price>" do
        addLot (Lot 10.0 now100) (Lot (-5.0) now200)
          @?= Just (ReduceLot (Left (Lot 5.0 now100))),
      testCase "add-lot-pos-neg>-price==" do
        addLot (Lot 10.0 now100) (Lot (-20.0) now100)
          @?= Just (ReduceLot (Right (Lot (-10.0) now100))),
      testCase "add-lot-pos-neg>-price<" do
        addLot (Lot 10.0 now100) (Lot (-20.0) now50)
          @?= Just (ReduceLot (Right (Lot (-10.0) now50))),
      testCase "add-lot-pos-neg>-price>" do
        addLot (Lot 10.0 now100) (Lot (-20.0) now200)
          @?= Just (ReduceLot (Right (Lot (-10.0) now200))),
      testCase "add-lot-neg-pos=-price==" do
        addLot (Lot (-10.0) now100) (Lot 10.0 now100)
          @?= Just (ReduceLot (Left (Lot 0 now100))),
      testCase "add-lot-neg-pos=-price<" do
        addLot (Lot (-10.0) now100) (Lot 10.0 now50)
          @?= Just (ReduceLot (Left (Lot 0 now100))),
      testCase "add-lot-neg-pos=-price>" do
        addLot (Lot (-10.0) now100) (Lot 10.0 now200)
          @?= Just (ReduceLot (Left (Lot 0 now100))),
      testCase "add-lot-neg-pos<-price==" do
        addLot (Lot (-10.0) now100) (Lot 5.0 now100)
          @?= Just (ReduceLot (Left (Lot (-5.0) now100))),
      testCase "add-lot-neg-pos<-price<" do
        addLot (Lot (-10.0) now100) (Lot 5.0 now50)
          @?= Just (ReduceLot (Left (Lot (-5.0) now100))),
      testCase "add-lot-neg-pos<-price>" do
        addLot (Lot (-10.0) now100) (Lot 5.0 now200)
          @?= Just (ReduceLot (Left (Lot (-5.0) now100))),
      testCase "add-lot-neg-pos>-price==" do
        addLot (Lot (-10.0) now100) (Lot 20.0 now100)
          @?= Just (ReduceLot (Right (Lot 10.0 now100))),
      testCase "add-lot-neg-pos>-price<" do
        addLot (Lot (-10.0) now100) (Lot 20.0 now50)
          @?= Just (ReduceLot (Right (Lot 10.0 now50))),
      testCase "add-lot-neg-pos>-price>" do
        addLot (Lot (-10.0) now100) (Lot 20.0 now200)
          @?= Just (ReduceLot (Right (Lot 10.0 now200)))
    ]

testAddToLots :: TestTree
testAddToLots =
  testGroup
    "addToLots"
    [ testCase "add-to-lots-pos-pos=-price==" do
        addToPositions
          id
          (Lot 10.0 now100)
          [Open (Lot 10.0 now100)]
          @?= [Open (Lot 20.00 now100)],
      testCase "add-to-lots-pos-many" do
        addToPositions
          id
          (Lot (-100.0) now200)
          [ Open (Lot 10.0 now100),
            Open (Lot 20.0 now100)
          ]
          @?= [ Closed (Lot 10.00 now100) now200,
                Closed (Lot 20.00 now100) now200,
                Open (Lot (-70.00) now200)
              ],
      testCase "add-to-lots-pos-few-fifo" do
        addToPositions
          id
          (Lot (-5) now200)
          [ Open (Lot 10.0 now100),
            Open (Lot 20.0 now100)
          ]
          @?= [ Open (Lot 5.00 now100),
                Closed (Lot 5.00 now100) now200,
                Open (Lot 20.00 now100)
              ],
      testCase "add-to-lots-pos-few-lifo" do
        addToPositions
          reverse
          (Lot (-5) now200)
          [ Open (Lot 10.0 now100),
            Open (Lot 20.0 now100)
          ]
          @?= [ Open (Lot 10.00 now100),
                Closed (Lot 5.00 now100) now200,
                Open (Lot 15.00 now100)
              ]
    ]

testIdentifyTrades :: TestTree
testIdentifyTrades =
  testGroup
    "identifyTrades"
    [ testCase "identify-trades-smoke" do
        identifyTrades mempty [("AAPL", Lot 10.0 now100)]
          @?= M.fromList [("AAPL", [Open (Lot 10.0 now100)])]
    ]

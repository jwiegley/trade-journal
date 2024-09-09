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

testAddLot :: TestTree
testAddLot =
  testGroup
    "addLot"
    [ testCase "add-lot-pos-pos=-price==" do
        now <- getCurrentTime
        addLot (Lot 10.0 100.0 now) (Lot 10.0 100.0 now)
          @?= IncreaseLot (Lot 20.0 100.0 now),
      testCase "add-lot-pos-pos=-price<" do
        now <- getCurrentTime
        addLot (Lot 10.0 100.0 now) (Lot 10.0 50.0 now)
          @?= AppendLots (Lot 10.0 100.0 now) (Lot 10.0 50.0 now),
      testCase "add-lot-pos-pos=-price>" do
        now <- getCurrentTime
        addLot (Lot 10.0 100.0 now) (Lot 10.0 200.0 now)
          @?= AppendLots (Lot 10.0 100.0 now) (Lot 10.0 200.0 now),
      testCase "add-lot-neg-neg=-price==" do
        now <- getCurrentTime
        addLot (Lot (-10.0) 100.0 now) (Lot (-10.0) 100.0 now)
          @?= IncreaseLot (Lot (-20.0) 100.0 now),
      testCase "add-lot-neg-neg=-price<" do
        now <- getCurrentTime
        addLot (Lot (-10.0) 100.0 now) (Lot (-10.0) 50.0 now)
          @?= AppendLots (Lot (-10.0) 100.0 now) (Lot (-10.0) 50.0 now),
      testCase "add-lot-neg-neg=-price>" do
        now <- getCurrentTime
        addLot (Lot (-10.0) 100.0 now) (Lot (-10.0) 200.0 now)
          @?= AppendLots (Lot (-10.0) 100.0 now) (Lot (-10.0) 200.0 now),
      testCase "add-lot-pos-neg=-price==" do
        now <- getCurrentTime
        addLot (Lot 10.0 100.0 now) (Lot (-10.0) 100.0 now)
          @?= CloseLot 0,
      testCase "add-lot-pos-neg=-price<" do
        now <- getCurrentTime
        addLot (Lot 10.0 100.0 now) (Lot (-10.0) 50.0 now)
          @?= CloseLot (-500),
      testCase "add-lot-pos-neg=-price>" do
        now <- getCurrentTime
        addLot (Lot 10.0 100.0 now) (Lot (-10.0) 200.0 now)
          @?= CloseLot 1000,
      testCase "add-lot-pos-neg<-price==" do
        now <- getCurrentTime
        addLot (Lot 10.0 100.0 now) (Lot (-5.0) 100.0 now)
          @?= ReduceLot (Lot 5.0 100.0 now) (Lot 5.0 100.0 now) 0,
      testCase "add-lot-pos-neg<-price<" do
        now <- getCurrentTime
        addLot (Lot 10.0 100.0 now) (Lot (-5.0) 50.0 now)
          @?= ReduceLot (Lot 5.0 100.0 now) (Lot 5.0 100.0 now) (-250),
      testCase "add-lot-pos-neg<-price>" do
        now <- getCurrentTime
        addLot (Lot 10.0 100.0 now) (Lot (-5.0) 200.0 now)
          @?= ReduceLot (Lot 5.0 100.0 now) (Lot 5.0 100.0 now) 500,
      testCase "add-lot-pos-neg>-price==" do
        now <- getCurrentTime
        addLot (Lot 10.0 100.0 now) (Lot (-20.0) 100.0 now)
          @?= ReplaceLot (Lot (-10.0) 100.0 now) (Lot 10.0 100.0 now) 0,
      testCase "add-lot-pos-neg>-price<" do
        now <- getCurrentTime
        addLot (Lot 10.0 100.0 now) (Lot (-20.0) 50.0 now)
          @?= ReplaceLot (Lot (-10.0) 50.0 now) (Lot 10.0 100.0 now) (-500),
      testCase "add-lot-pos-neg>-price>" do
        now <- getCurrentTime
        addLot (Lot 10.0 100.0 now) (Lot (-20.0) 200.0 now)
          @?= ReplaceLot (Lot (-10.0) 200.0 now) (Lot 10.0 100.0 now) 1000,
      testCase "add-lot-neg-pos=-price==" do
        now <- getCurrentTime
        addLot (Lot (-10.0) 100.0 now) (Lot 10.0 100.0 now)
          @?= CloseLot 0,
      testCase "add-lot-neg-pos=-price<" do
        now <- getCurrentTime
        addLot (Lot (-10.0) 100.0 now) (Lot 10.0 50.0 now)
          @?= CloseLot 500,
      testCase "add-lot-neg-pos=-price>" do
        now <- getCurrentTime
        addLot (Lot (-10.0) 100.0 now) (Lot 10.0 200.0 now)
          @?= CloseLot (-1000),
      testCase "add-lot-neg-pos<-price==" do
        now <- getCurrentTime
        addLot (Lot (-10.0) 100.0 now) (Lot 5.0 100.0 now)
          @?= ReduceLot (Lot (-5.0) 100.0 now) (Lot (-5.0) 100.0 now) 0,
      testCase "add-lot-neg-pos<-price<" do
        now <- getCurrentTime
        addLot (Lot (-10.0) 100.0 now) (Lot 5.0 50.0 now)
          @?= ReduceLot (Lot (-5.0) 100.0 now) (Lot (-5.0) 100.0 now) 250,
      testCase "add-lot-neg-pos<-price>" do
        now <- getCurrentTime
        addLot (Lot (-10.0) 100.0 now) (Lot 5.0 200.0 now)
          @?= ReduceLot (Lot (-5.0) 100.0 now) (Lot (-5.0) 100.0 now) (-500),
      testCase "add-lot-neg-pos>-price==" do
        now <- getCurrentTime
        addLot (Lot (-10.0) 100.0 now) (Lot 20.0 100.0 now)
          @?= ReplaceLot (Lot 10.0 100.0 now) (Lot (-10.0) 100.0 now) 0,
      testCase "add-lot-neg-pos>-price<" do
        now <- getCurrentTime
        addLot (Lot (-10.0) 100.0 now) (Lot 20.0 50.0 now)
          @?= ReplaceLot (Lot 10.0 50.0 now) (Lot (-10.0) 100.0 now) 500,
      testCase "add-lot-neg-pos>-price>" do
        now <- getCurrentTime
        addLot (Lot (-10.0) 100.0 now) (Lot 20.0 200.0 now)
          @?= ReplaceLot (Lot 10.0 200.0 now) (Lot (-10.0) 100.0 now) (-1000)
    ]

testAddToLots :: TestTree
testAddToLots =
  testGroup
    "addToLots"
    [ testCase "add-to-lots-pos-pos=-price==" do
        now <- getCurrentTime
        addToPositions
          FIFO
          (Lot 10.0 100.0 now)
          [Open (Lot 10.0 100.0 now)]
          @?= [Open (Lot 20.00 100.00 now)],
      testCase "add-to-lots-pos-many" do
        now <- getCurrentTime
        addToPositions
          FIFO
          (Lot (-100.0) 200.0 now)
          [ Open (Lot 10.0 100.0 now),
            Open (Lot 20.0 100.0 now)
          ]
          @?= [ Closed (Lot 10.00 100.00 now) 1000.00,
                Closed (Lot 20.00 100.00 now) 2000.00,
                Open (Lot (-70.00) 200.00 now)
              ],
      testCase "add-to-lots-pos-few-fifo" do
        now <- getCurrentTime
        addToPositions
          FIFO
          (Lot (-5) 200.0 now)
          [ Open (Lot 10.0 100.0 now),
            Open (Lot 20.0 100.0 now)
          ]
          @?= [ Closed (Lot 5.00 100.00 now) 500.00,
                Open (Lot 5.00 100.00 now),
                Open (Lot 20.00 100.00 now)
              ],
      testCase "add-to-lots-pos-few-lifo" do
        now <- getCurrentTime
        addToPositions
          LIFO
          (Lot (-5) 200.0 now)
          [ Open (Lot 10.0 100.0 now),
            Open (Lot 20.0 100.0 now)
          ]
          @?= [ Open (Lot 10.00 100.00 now),
                Open (Lot 15.00 100.00 now),
                Closed (Lot 5.00 100.00 now) 500.00
              ]
    ]

testIdentifyTrades :: TestTree
testIdentifyTrades =
  testGroup
    "identifyTrades"
    [ testCase "identify-trades-smoke" do
        now <- getCurrentTime
        identifyTrades [("AAPL", Lot 10.0 100.0 now)]
          @?= M.fromList [("AAPL", [Open (Lot 10.0 100.0 now)])]
    ]

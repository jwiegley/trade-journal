{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module TestWashSaleRule2 where

import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Map qualified as M
import Data.Time
import Test.Tasty
import Test.Tasty.HUnit
import Text.Show.Pretty (ppShow)
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
          @?= AddLot (Lot 20 now100),
      testCase "add-lot-pos-pos=-price<" do
        addLot (Lot 10 now100) (Lot 10 now50)
          @?= NoChange,
      testCase "add-lot-pos-pos=-price>" do
        addLot (Lot 10 now100) (Lot 10 now200)
          @?= NoChange,
      testCase "add-lot-neg-neg=-price==" do
        addLot (Lot (-10) now100) (Lot (-10) now100)
          @?= AddLot (Lot (-20) now100),
      testCase "add-lot-neg-neg=-price<" do
        addLot (Lot (-10) now100) (Lot (-10) now50)
          @?= NoChange,
      testCase "add-lot-neg-neg=-price>" do
        addLot (Lot (-10) now100) (Lot (-10) now200)
          @?= NoChange,
      testCase "add-lot-pos-neg=-price==" do
        addLot (Lot 10 now100) (Lot (-10) now100)
          @?= ReduceLot (Lot 0 now100),
      testCase "add-lot-pos-neg=-price<" do
        addLot (Lot 10 now100) (Lot (-10) now50)
          @?= ReduceLot (Lot 0 now100),
      testCase "add-lot-pos-neg=-price>" do
        addLot (Lot 10 now100) (Lot (-10) now200)
          @?= ReduceLot (Lot 0 now100),
      testCase "add-lot-pos-neg<-price==" do
        addLot (Lot 10 now100) (Lot (-5) now100)
          @?= ReduceLot (Lot 5 now100),
      testCase "add-lot-pos-neg<-price<" do
        addLot (Lot 10 now100) (Lot (-5) now50)
          @?= ReduceLot (Lot 5 now100),
      testCase "add-lot-pos-neg<-price>" do
        addLot (Lot 10 now100) (Lot (-5) now200)
          @?= ReduceLot (Lot 5 now100),
      testCase "add-lot-pos-neg>-price==" do
        addLot (Lot 10 now100) (Lot (-20) now100)
          @?= ReplaceLot (Lot (-10) now100),
      testCase "add-lot-pos-neg>-price<" do
        addLot (Lot 10 now100) (Lot (-20) now50)
          @?= ReplaceLot (Lot (-10) now50),
      testCase "add-lot-pos-neg>-price>" do
        addLot (Lot 10 now100) (Lot (-20) now200)
          @?= ReplaceLot (Lot (-10) now200),
      testCase "add-lot-neg-pos=-price==" do
        addLot (Lot (-10) now100) (Lot 10 now100)
          @?= ReduceLot (Lot 0 now100),
      testCase "add-lot-neg-pos=-price<" do
        addLot (Lot (-10) now100) (Lot 10 now50)
          @?= ReduceLot (Lot 0 now100),
      testCase "add-lot-neg-pos=-price>" do
        addLot (Lot (-10) now100) (Lot 10 now200)
          @?= ReduceLot (Lot 0 now100),
      testCase "add-lot-neg-pos<-price==" do
        addLot (Lot (-10) now100) (Lot 5 now100)
          @?= ReduceLot (Lot (-5) now100),
      testCase "add-lot-neg-pos<-price<" do
        addLot (Lot (-10) now100) (Lot 5 now50)
          @?= ReduceLot (Lot (-5) now100),
      testCase "add-lot-neg-pos<-price>" do
        addLot (Lot (-10) now100) (Lot 5 now200)
          @?= ReduceLot (Lot (-5) now100),
      testCase "add-lot-neg-pos>-price==" do
        addLot (Lot (-10) now100) (Lot 20 now100)
          @?= ReplaceLot (Lot 10 now100),
      testCase "add-lot-neg-pos>-price<" do
        addLot (Lot (-10) now100) (Lot 20 now50)
          @?= ReplaceLot (Lot 10 now50),
      testCase "add-lot-neg-pos>-price>" do
        addLot (Lot (-10) now100) (Lot 20 now200)
          @?= ReplaceLot (Lot 10 now200)
    ]

testAddToLots :: TestTree
testAddToLots =
  testGroup
    "addToPositions"
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

infix 1 @?==

(@?==) :: (Eq a, Show a, HasCallStack, MonadIO m) => a -> a -> m ()
actual @?== expected = liftIO $ assertEqual' "" expected actual

assertEqual' :: (Eq a, Show a, HasCallStack) => String -> a -> a -> Assertion
assertEqual' preface expected actual =
  unless (actual == expected) $ assertFailure msg
  where
    msg =
      (if null preface then "" else preface ++ "\n")
        ++ "expected: "
        ++ ppShow expected
        ++ "\n but got: "
        ++ ppShow actual

testWashSales :: TestTree
testWashSales =
  testGroup
    "washSales"
    [ testCase "wash-sales-noop" do
        washSales [open 10 100 0 Nothing] @?= [open 10 100 0 Nothing],
      testCase "wash-sales-buy-sell-buy" do
        washSales
          [ open 10 100 0 Nothing,
            closed 10 100 0 50 10 True,
            open 10 70 20 Nothing
          ]
          @?= [ open 10 100 0 Nothing,
                closed 10 100 0 100 10 False,
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
                closed 5 100 0 100 20 False
              ],
      testCase "wash-sales-brian-wruk" do
        washSales
          ( addManyToPositions
              id
              [ Lot 10 (TimePrice 100 (sometime 1)),
                Lot (-10) (TimePrice 110 (sometime 2)), -- profit
                Lot 10 (TimePrice 100 (sometime 3)),
                Lot (-10) (TimePrice 90 (sometime 4)), -- loss
                Lot 10 (TimePrice 100 (sometime 5)),
                Lot (-10) (TimePrice 110 (sometime 6)), -- profit
                Lot 10 (TimePrice 100 (sometime 7)),
                Lot (-10) (TimePrice 90 (sometime 8)), -- loss
                Lot 10 (TimePrice 100 (sometime 9)),
                Lot (-10) (TimePrice 110 (sometime 10)), -- profit
                Lot 10 (TimePrice 100 (sometime 11)),
                Lot (-10) (TimePrice 90 (sometime 12)), -- loss
                Lot 10 (TimePrice 100 (sometime 13)),
                Lot (-10) (TimePrice 110 (sometime 14)), -- profit
                Lot 10 (TimePrice 100 (sometime 15)),
                Lot (-10) (TimePrice 90 (sometime 16)), -- loss
                Lot 10 (TimePrice 100 (sometime 17)),
                Lot (-10) (TimePrice 110 (sometime 18)), -- profit
                Lot 10 (TimePrice 100 (sometime 19))
              ]
              []
          )
          @?== [ closed 10 100 1 110 2 False,
                 closed 10 100 3 100 4 False,
                 closed 10 100 5 110 6 False,
                 closed 10 100 7 100 8 False,
                 closed 10 100 9 110 10 False,
                 closed 10 100 11 100 12 False,
                 closed 10 100 13 110 14 False,
                 closed 10 100 15 100 16 False,
                 closed 10 100 17 110 18 False,
                 open
                   10
                   100
                   4
                   (Just (100 + (4 * 10 * (100 - 90)) / 10))
               ]
    ]
  where
    open n p d =
      Open (Lot n (TimePrice p (sometime d)))
    closed n p d p' d' =
      Closed
        (Lot n (TimePrice p (sometime d)))
        (TimePrice p' (sometime d'))

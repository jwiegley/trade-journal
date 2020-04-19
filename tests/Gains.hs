{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Gains (testGainsKeeper) where

import Control.Lens
import Data.Amount
import Data.Coerce
import Data.Ledger as Ledger
import Data.Maybe (fromMaybe)
-- import Data.Ratio
-- import Data.Time
-- import Hedgehog
-- import Hedgehog.Gen as Gen
-- import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.Hedgehog
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import ThinkOrSwim.Gains
import ThinkOrSwim.Types

pl :: Amount 4 -> CommodityLot t -> CommodityLot t -> Amount 2
pl q x y = coerce $
   q * (fromMaybe 0 (y^.Ledger.cost) / abs (y^.quantity) -
        fromMaybe 0 (x^.Ledger.cost) / abs (x^.quantity))

testGainsKeeper :: TestTree
testGainsKeeper = testGroup "gainsKeeper"
    [ testCase "12@@300 `applyLots` -10@@500" $
      q12c300 `closeLot` qn10c500
          @?= LotApplied
                (pl (-10) q12c300 qn10c500)
                (Some ((-10)@@(10 * (300/12)))
                      (2@@(2 * (300/12))))
                (All qn10c500)

    , testCase "10@@500 `closeLot` -12@@300" $
      q10c500 `closeLot` qn12c300
          @?= LotApplied
                (pl (-10) qn10c500 q12c300)
                (All ((-10)@@(10 * (500/10))))
                (Some ((-10)@@(10 * (300/12)))
                      ((-2)@@(2 * (300/12))))

    , testCase "12@@500 `closeLot` -10@@300" $
      q12c500 `closeLot` qn10c300
          @?= LotApplied
                (pl (-10) q12c500 qn10c300)
                (Some ((-10)@@(10 * (500/12)))
                      (2@@(2 * (500/12))))
                (All qn10c300)

    , testCase "10@@300 `closeLot` -12@@500" $
      q10c300 `closeLot` qn12c500
          @?= LotApplied
                (pl (-10) qn10c300 q12c500)
                (All ((-10)@@(10 * (300/10))))
                (Some ((-10)@@(10 * (500/12)))
                      ((-2)@@83.33333333333333))

    , testCase "-10@@300 `closeLot` 12@@500" $
      qn10c300 `closeLot` q12c500
          @?= LotApplied
                (pl 10 qn10c300 q12c500)
                (All (10@@(10 * (300/10))))
                (Some (10@@(10 * (500/12)))
                      (2@@(2 * (500/12))))

    , testCase "-10@@500 `closeLot` 12@@300" $
      qn10c500 `closeLot` q12c300
          @?= LotApplied
                (pl 10 qn10c500 q12c300)
                (All (10@@(10 * (500/10))))
                (Some (10@@(10 * (300/12)))
                      (2@@(2 * (300/12))))

    , testCase "calculateGains -400" $
      calculatePL
          ((-400) @@ 69727.28)
          [ 100 @@ 17350.00
          , 400 @@ 69722.60
          ]
          @?= CalculatedPL
                  [ (-100) @@ 17350.00 $$$ (-81.82)
                  , (-300) @@ 52291.95000000001 $$$ (-3.51) ]
                  [ 100 @@ 17430.65 ]
                  []

    , testCase "calculatePL 400" $
      calculatePL
          (400 @@ 69722.60)
          [ 100 @@ 17350.00
          ]
          @?= CalculatedPL
                  []
                  [ 100 @@ 17350.00 ]
                  [ 400 @@ 69722.60 ]

    , testCase "calculatePL SNAP" $
      calculatePL
          ((-11.0) @@ 189.97)
          [ 700.0 @@ 12053.72
          , 300.0 @@ 5165.97
          ]
      @?= CalculatedPL
              [ (-11) @@ 189.41559999999998 $$$ (-0.5544) ]
              [ 689.0 @@ 11864.304399999999
              , 300.0 @@ 5165.97 ]
              []

    , testCase "handleFees opening position" $
      handleFees @API.Transaction
          0.81 [ 100 @@ 1000.00 $$$ 0.0 ]
          @?= [ 100 @@ 1000.81 $$$ 0.0 ]

    , testCase "handleFees closing single position" $
      handleFees @API.Transaction
          0.81 [ (-100) @@ 1000.81 $$$ 199.19 ]
          @?= [ (-100) @@ 1000.81 $$$ 200.00 ]

    , testCase "handleFees closing multiple positions 1" $
      handleFees @API.Transaction
          0.81 [ (-100) @@ 1000.81 $$$ (-100.00)
               , (-100) @@ 1000.81 $$$ (-100.00)
               ]
          @?= [ (-100) @@ 1000.81 $$$ (-100.00 + 0.40)
              , (-100) @@ 1000.81 $$$ (-100.00 + 0.40)
              ]

    , testCase "handleFees closing multiple positions 2" $
      handleFees @API.Transaction
          0.83 [ (-10.00) @@ 19740.50 $$$ (-100.00)
               , (-10.00) @@ 19707.90 $$$ (-100.00)
               ]
          @?= [ (-10.00) @@ 19740.50 $$$ (-100.00 + 0.41)
              , (-10.00) @@ 19707.90 $$$ (-100.00 + 0.41)
              ]

    , testCase "handleFees closing multiple positions 3" $
      handleFees @API.Transaction
          0.47 [ (-689.00) @@ 11864.3044 $$$ (-34.87)
               , (-300.00) @@ 5165.97 $$$ (-15.09)
               ]
          @?= [ (-689.00) @@ 11864.3044 $$$ (-34.54)
              , (-300.00) @@ 5165.97 $$$ (-14.95)
              ]

    -- , testCase "sumLotAndPL 1" $
    --   sumLotAndPL [ (-22.00) @@ (22 * 307.772887) $$$  (-4.85)
    --               , (-75.00) @@ (75 * 331.241137) $$$ 1743.57
    --               ,  (-3.00) @@ ( 3 * 307.745049) $$$  (-0.75)
    --               ]
    --       @?= (-30799.35)       -- Ledger's answer

    -- , testCase "sumLotAndPL 2" $
    --   sumLotAndPL [ 78.00 @@ (78 * 308.5035)  $$$     0.00
    --               , 22.00 @@ (22 * 326.87785) $$$ (-404.24)
    --               ]
    --       @?= 30850.35

    -- , testCase "sumLotAndPL 3" $
    --   sumLotAndPL [ (-100.00) @@ (100 * 307.745049) $$$ (-338.09)
    --               ,  (-78.00) @@ ( 78 * 308.5035)   $$$ (-204.55)
    --               ,  (-22.00) @@ ( 22 * 326.87785)  $$$   346.54
    --               ]
    --       @?= (-62225.19)

    -- , testCase "sumLotAndPL 4" $
    --   sumLotAndPL [ 144.00 @@ (144 * 311.8644) $$$ (-18.02)
    --               ,  56.00 @@ ( 56 * 312.1861) $$$    0.00
    --               ]
    --       @?= 62372.88

    -- , testCase "sumLotAndPL 5" $
    --   sumLotAndPL [ (-144.00) @@ (144 * 311.8644) $$$ 174.87
    --               ,  (-56.00) @@ ( 56 * 312.1861) $$$  86.02
    --               ]
    --       @?= (-62130.01)

    , testCase "sumLotAndPL 6" $
      sumLotAndPL [ 112.00 @@ (112 * 309.7473) $$$ 0.00
                  ,  88.00 @@ ( 88 * 309.7521) $$$ (-0.42)
                  ]
          @?= 61949.46

    , testCase "sumLotAndPL 7" $
      sumLotAndPL [ (-34.00) @@ (34 * 309.7473) $$$ 11.41
                  , (-78.00) @@ (78 * 309.7473) $$$ 26.96
                  , (-88.00) @@ (88 * 309.7521) $$$ 30.84
                  ]
          @?= (-61880.67)

    , testCase "sumLotAndPL 8" $
      sumLotAndPL [ (-90.00) @@ (90 * 99.7792) $$$ 1170.41
                  , (-10.00) @@ (10 * 89.785)  $$$   30.10
                  ]
          @?= (-8677.47)

    -- , testProperty "closeLot" $ property $ do
    --       x <- forAll genCommodityLot
    --       y <- forAll genCommodityLot
    --       let (_gain, (used, kept), _left) = x `closeLot` y
    --       Hedgehog.assert $ isJust used || isJust kept
    --       Prelude.maybe 0 lotCost used + Prelude.maybe 0 lotCost kept
    --           === lotCost x
    ]
  where
    q12c300  =    12 @@ 300
    qn12c300 = (-12) @@ 300
    q10c500  =    10 @@ 500
    qn10c500 = (-10) @@ 500
    q12c500  =    12 @@ 500
    qn12c500 = (-12) @@ 500
    q10c300  =    10 @@ 300
    qn10c300 = (-10) @@ 300

{-
genAmount :: MonadGen m => m (Amount n)
genAmount = Amount <$> liftM2 (%)
    (integral (Range.constantFrom 100 (-1000) 1000))
    (integral (Range.constantFrom 100 1 1000))

genCommodityLot :: MonadGen m => m CommodityLot
genCommodityLot = do
    _instrument   <- enumBounded
    _quantity     <- genAmount
    _symbol       <- pure "???"
    _cost         <- Just <$> genAmount
    _purchaseDate <- Just <$> genUTCTime
    _refs         <- pure []
    _price        <- Just <$> genAmount
    pure CommodityLot {..}

genUTCTime :: MonadGen m => m UTCTime
genUTCTime = do
    y <- toInteger <$> Gen.int (Range.constant 2000 2019)
    m <- Gen.int (Range.constant 1 12)
    d <- Gen.int (Range.constant 1 28)
    let day = fromGregorian y m d
    secs <- toInteger <$> Gen.int (Range.constant 0 86401)
    let delta = secondsToDiffTime secs
    pure $ UTCTime day delta
-}

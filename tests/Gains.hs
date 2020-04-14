{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Gains where

import Control.Lens
import Control.Monad.State
import Data.Amount
import Data.Ledger
-- import Data.Maybe (isJust)
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

testApplyLots :: TestTree
testApplyLots = testGroup "Gains"
    [ testCase "12@@300 `applyLots` -10@@500" $
      (12@@300) `applyLot` ((-10)@@500)
          @?= LotApplied
                (((-10.0) * ((500 / 10) - (300 / 12)))^.from (rounded @2 @2))
                (Just ((-10)@@(10 * (300/12))))
                (Just (2@@(2 * (300/12))))
                Nothing

    , testCase "10@@500 `applyLot` -12@@300" $
      (10@@500) `applyLot` ((-12)@@300)
          @?= LotApplied
                (((-10.0) * ((300 / 12) - (500 / 10)))^.from (rounded @2 @2))
                (Just ((-10)@@(10 * (500/10))))
                Nothing
                (Just ((-2)@@(2 * (300/12))))

    , testCase "12@@500 `applyLot` -10@@300" $
      (12@@500) `applyLot` ((-10)@@300)
          @?= LotApplied
                (((-10.0) * ((300 / 10) - (500 / 12)))^.from (rounded @2 @2))
                (Just ((-10)@@(10 * (500/12))))
                (Just (2@@(2 * (500/12))))
                Nothing

    , testCase "10@@300 `applyLot` -12@@500" $
      (10@@300) `applyLot` ((-12)@@500)
          @?= LotApplied
                (-116.666667)
                (Just ((-10)@@(10 * (300/10))))
                Nothing
                (Just ((-2)@@83.33333333333333))

    , testCase "-10@@300 `applyLot` 12@@500" $
      ((-10)@@300) `applyLot` (12@@500)
          @?= LotApplied
                116.666667
                (Just (10@@(10 * (300/10))))
                Nothing
                (Just (2@@(2 * (500/12))))

    , testCase "-10@@500 `applyLot` 12@@300" $
      ((-10)@@500) `applyLot` (12@@300)
          @?= LotApplied
                ((10.0 * ((300 / 12) - (500 / 10)))^.from (rounded @2 @2))
                (Just (10@@(10 * (500/10))))
                Nothing
                (Just (2@@(2 * (300/12))))

    , testCase "calculateGains -400" $
      let res = flip evalState newGainsKeeperState $ calculatePL
              ((-400) @@ 69727.28) -- {174.3182}
              [ 100 @@ 17350.00    -- {173.50}
              , 400 @@ 69722.60    -- {174.3065}
              ]
      in res @?= CalculatedPL
                   [ LotAndPL (-81.82) ((-100) @@ 17350.00)
                   , LotAndPL (-3.51) ((-300) @@ 52291.95000000001) ]
                   [100 @@ 17430.65]
                   Nothing

    , testCase "calculatePL 400" $
      let res = flip evalState newGainsKeeperState $ calculatePL
              (400 @@ 69722.60)
              [ 100 @@ 17350.00
              ]
      in res @?= CalculatedPL
                   []
                   [ 100 @@ 17350.00 ]
                   (Just (400 @@ 69722.60))

    , testCase "calculatePL SNAP" $
      let res = flip evalState newGainsKeeperState $ calculatePL
              ((-11.0) @@ 189.97)
              [ 700.0 @@ 12053.72
              , 300.0 @@ 5165.97
              ]
      in res @?= CalculatedPL
                   [ LotAndPL (-0.5544) ((-11) @@ 189.41559999999998) ]
                   [ 689.0 @@ 11864.304399999999
                   , 300.0 @@ 5165.97 ]
                   Nothing

    , testCase "handleFees opening position" $
      handleFees @API.Transaction
          0.81 [ LotAndPL 0.0 (100 @@ 1000.00) ]
          @?= [ LotAndPL 0.0 (100 @@ 1000.81) ]

    , testCase "handleFees closing single position" $
      handleFees @API.Transaction
          0.81 [ LotAndPL 199.19 ((-100) @@ 1000.81) ]
          @?= [ LotAndPL 200.00 ((-100) @@ 1000.81) ]

    , testCase "handleFees closing multiple positions 1" $
      handleFees @API.Transaction
          0.81 [ LotAndPL (-100.00) ((-100) @@ 1000.81)
               , LotAndPL (-100.00) ((-100) @@ 1000.81)
               ]
          @?= [ LotAndPL (-100.00 + 0.40 + 0.01) ((-100) @@ 1000.81)
              , LotAndPL (-100.00 + 0.40) ((-100) @@ 1000.81)
              ]

    , testCase "handleFees closing multiple positions 2" $
      handleFees @API.Transaction
          0.83 [ LotAndPL (-100.00) ((-10.00) @@ 19740.50)
               , LotAndPL (-100.00) ((-10.00) @@ 19707.90)
               ]
          @?= [ LotAndPL (-100.00 + 0.41 + 0.01) ((-10.00) @@ 19740.50)
              , LotAndPL (-100.00 + 0.41) ((-10.00) @@ 19707.90)
              ]

    , testCase "handleFees closing multiple positions 3" $
      handleFees @API.Transaction
          0.47 [ LotAndPL (-34.87) ((-689.00) @@ 11864.3044)
               , LotAndPL (-15.09) ((-300.00) @@ 5165.97)
               ]
          @?= [ LotAndPL (-34.54) ((-689.00) @@ 11864.3044)
              , LotAndPL (-14.95) ((-300.00) @@ 5165.97)
              ]

    -- , testProperty "applyLot" $ property $ do
    --       x <- forAll genCommodityLot
    --       y <- forAll genCommodityLot
    --       let (_gain, (used, kept), _left) = x `applyLot` y
    --       Hedgehog.assert $ isJust used || isJust kept
    --       Prelude.maybe 0 lotCost used + Prelude.maybe 0 lotCost kept
    --           === lotCost x
    ]

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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Gains where

import Control.Lens
import Data.Amount
import Test.Tasty
import Test.Tasty.HUnit
import ThinkOrSwim.Gains

testApplyLots :: TestTree
testApplyLots = testGroup "Gains"
    [ testCase "12@@300 `applyLots` -10@@500" $
      (12@@300) `applyLots` ((-10)@@500)
          @?= ( (10.0 * ((500 / 10) - (300 / 12)))^.from (rounded @2 @2)
              , ( Just ((-10)@@(10 * (300/12)))
                , Just (2@@(2 * (300/12))))
              , Nothing)

    , testCase "10@@500 `applyLots` -12@@300" $
      (10@@500) `applyLots` ((-12)@@300)
          @?= ( (10.0 * ((300 / 12) - (500 / 10)))^.from (rounded @2 @2)
              , ( Just ((-10)@@(10 * (500/10)))
                , Nothing)
              , Just ((-2)@@(2 * (300/12))))

    , testCase "12@@500 `applyLots` -10@@300" $
      (12@@500) `applyLots` ((-10)@@300)
          @?= ( (10.0 * ((300 / 10) - (500 / 12)))^.from (rounded @2 @2)
              , ( Just ((-10)@@(10 * (500/12)))
                , Just (2@@(2 * (500/12))))
              , Nothing)

    , testCase "10@@300 `applyLots` -12@@500" $
      (10@@300) `applyLots` ((-12)@@500)
          @?= ( 116.67
              , ( Just ((-10)@@(10 * (300/10)))
                , Nothing)
              , Just ((-2)@@83.33333333333333))

    , testCase "-10@@300 `applyLots` 12@@500" $
      ((-10)@@300) `applyLots` (12@@500)
          @?= ( -116.67
              , ( Just (10@@(10 * (300/10)))
                , Nothing)
              , Just (2@@(2 * (500/12))))

    , testCase "-10@@500 `applyLots` 12@@300" $
      ((-10)@@500) `applyLots` (12@@300)
          @?= ( ((-10.0) * ((300 / 12) - (500 / 10)))^.from (rounded @2 @2)
              , ( Just (10@@(10 * (500/10)))
                , Nothing)
              , Just (2@@(2 * (300/12))))

    , testCase "calculateGains -400" $
      calculateGains
          ((-400) @@ 69727.28) -- {174.3182}
          [ 100 @@ 17350.00    -- {173.50}
          , 400 @@ 69722.60    -- {174.3065}
          ]
          @?= ( [ (81.82, (-100) @@ 17350.00)
                , (3.51, (-300) @@ 52291.95000000001) ]
              , [100 @@ 17430.65] )

    , testCase "calculateGains 400" $
      calculateGains
          (400 @@ 69722.60)
          [ 100 @@ 17350.00
          ]
          @?= ( [ (0.0, 400 @@ 69722.60) ]
              , [ 100 @@ 17350.00
                , 400 @@ 69722.60
                ] )

    , testCase "calculateGains SNAP" $
      calculateGains
          ((-11.0) @@ 189.97)
          [ 700.0 @@ 12053.72
          , 300.0 @@ 5165.97
          ]
          @?= ( [ (0.55, (-11) @@ 189.41559999999998) ]
              , [ 689.0 @@ 11864.304399999999
                , 300.0 @@ 5165.97 ] )
    ]

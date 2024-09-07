-- {-# LANGUAGE BlockArguments #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE ImportQualifiedPost #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE ViewPatterns #-}

module TestWashSaleRule2 where

-- import Amount
-- import Control.Lens hiding (Context)
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
    [ testCase "add-lot-pos-pos" $ do
        now <- getCurrentTime
        addLot (Lot 10.0 100.0 now) (Lot 10.0 100.0 now)
          @?= AddResult Nothing (Just [Lot 20.0 100.0 now])
          -- ,
          --   testCase "add-lot-neg-neg" undefined,
          --   testCase "add-lot-pos-neg=" undefined,
          --   testCase "add-lot-pos-neg<" undefined,
          --   testCase "add-lot-pos-neg>" undefined,
          --   testCase "add-lot-neg-pos=" undefined,
          --   testCase "add-lot-neg-pos<" undefined,
          --   testCase "add-lot-neg-pos>" undefined
    ]

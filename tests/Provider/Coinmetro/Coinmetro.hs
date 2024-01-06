{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE QuasiQuotes #-}

module Coinmetro (testCoinmetro) where

-- import Data.HashMap.Strict (fromList)
-- import qualified Data.Map as Map
-- import Data.String.Here.Interpolated
-- import Journal.Coinmetro
import Test.Tasty
import Test.Tasty.HUnit

testCoinmetro :: TestTree
testCoinmetro =
  testGroup
    "coinmetro"
    [ testCase "import" testCsvImport
    ]

testCsvImport :: Assertion
testCsvImport = pure ()

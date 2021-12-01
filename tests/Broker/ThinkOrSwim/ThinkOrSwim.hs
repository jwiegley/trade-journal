{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE QuasiQuotes #-}

module ThinkOrSwim (testThinkOrSwim) where

-- import Data.HashMap.Strict (fromList)
-- import qualified Data.Map as Map
-- import Data.String.Here.Interpolated
-- import Journal.ThinkOrSwim
import Test.Tasty
import Test.Tasty.HUnit

testThinkOrSwim :: TestTree
testThinkOrSwim =
  testGroup
    "thinkorswim"
    [ testCase "import" testCsvImport
    ]

testCsvImport :: Assertion
testCsvImport = pure ()

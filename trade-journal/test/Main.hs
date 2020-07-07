module Main where

-- import Examples
-- import ModelTests
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "trade-journal"
      []

-- testExamples,
-- testModel

module Main where

import Examples
import ModelTests
import Test.Tasty
import ThinkOrSwim

main :: IO ()
main =
  defaultMain $
    testGroup
      "trade-journal"
      [ testExamples,
        testModel,
        testThinkOrSwim
      ]

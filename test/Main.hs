module Main where

import Examples
import Gains
import GainsKeeper
import Test.Tasty
import ThinkOrSwim

main :: IO ()
main =
  defaultMain $
    testGroup
      "trade-journal"
      [ testGains,
        testGainsKeeper,
        testExamples,
        testThinkOrSwim
      ]

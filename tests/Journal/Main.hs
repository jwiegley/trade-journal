module Main where

import Closings
-- import Examples
-- import Gains
-- import GainsKeeper
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "journal"
      [ testClosings
      -- testGains,
      -- testGainsKeeper,
      -- testExamples
      ]

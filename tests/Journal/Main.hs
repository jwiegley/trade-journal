module Main where

import Closings
-- import Examples
-- import Gains
-- import GainsKeeper
import Test.Tasty
import WashSaleRule

main :: IO ()
main =
  defaultMain $
    testGroup
      "journal"
      [ testClosings,
        testWashSaleRule
        -- testGains,
        -- testGainsKeeper,
        -- testExamples
      ]

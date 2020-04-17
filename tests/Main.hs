module Main where

import Gains
import Test.Tasty
import Wash

main :: IO ()
main = defaultMain $ testGroup "thinkorswim"
    [ testGainsKeeper
    , testWashSaleRule
    ]

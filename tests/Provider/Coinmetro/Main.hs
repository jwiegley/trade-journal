module Main where

import Coinmetro
import Test.Tasty

main :: IO ()
main =
    defaultMain $
        testGroup
            "coinmetro"
            [ testCoinmetro
            ]

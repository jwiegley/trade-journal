module Main where

import Test.Tasty
import Coinmetro

main :: IO ()
main =
  defaultMain $
    testGroup
      "coinmetro"
      [ testCoinmetro
      ]

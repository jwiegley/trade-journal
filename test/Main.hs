module Main where

import ModelTests
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "trade-journal"
      [ testModel
      ]

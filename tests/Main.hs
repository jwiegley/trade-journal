module Main where

import Model
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "thinkorswim"
      [ testModel
      ]

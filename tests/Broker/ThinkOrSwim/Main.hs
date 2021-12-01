module Main where

import Test.Tasty
import ThinkOrSwim

main :: IO ()
main =
  defaultMain $
    testGroup
      "thinkorswim"
      [ testThinkOrSwim
      ]

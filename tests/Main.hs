module Main where

import Gains
import Mock
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "thinkorswim"
    [ testMock                  -- make sure the basic machinery works
    , testGains
    ]

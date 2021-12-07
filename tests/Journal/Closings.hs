{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

module Closings where

import Control.Lens hiding (each)
import Hedgehog hiding (Action)
import Journal.Closings
import Journal.Types
import Test.Tasty
import Test.Tasty.Hedgehog
import TestAction

testClosings :: TestTree
testClosings =
  testGroup
    "closings"
    [ testProperty "buy-buy-buy" $
        property $ do
          b <- forAll $ genAnnotated genLot
          checkJournal
            id
            do
              buy b
              buy b
              buy b
            do
              buy b
              open 1 Long b
              buy b
              open 2 Long b
              buy b
              open 3 Long b
            do
              open 1 Long b
              open 2 Long b
              open 3 Long b,
      --
      testProperty "buy-buy-buy-sell-sell-sell" $
        property $ do
          b <- forAll $ genAnnotated genLot
          checkJournal
            id
            do
              buy b
              buy b
              buy b
              sell b
              sell b
              sell b
            do
              buy b
              open 1 Long b
              buy b
              open 2 Long b
              buy b
              open 3 Long b
              sell b
              close 1 b
              sell b
              close 2 b
              sell b
              close 3 b
            do
              pure (),
      --
      testProperty "buy-buy-buy-sell2" $
        property $ do
          b <- forAll $ genAnnotated genLot
          let b2 = b & item . amount *~ 2
          checkJournal
            id
            do
              buy b
              buy b
              buy b
              sell b2
            do
              buy b
              open 1 Long b
              buy b
              open 2 Long b
              buy b
              open 3 Long b
              sell b2
              close 1 b
              close 2 b
            do
              open 3 Long b,
      --
      testProperty "buy-buy-buy-sell3" $
        property $ do
          b <- forAll $ genAnnotated genLot
          let b3 = b & item . amount *~ 3
          checkJournal
            id
            do
              buy b
              buy b
              buy b
              sell b3
            do
              buy b
              open 1 Long b
              buy b
              open 2 Long b
              buy b
              open 3 Long b
              sell b3
              close 1 b
              close 2 b
              close 3 b
            do
              pure (),
      --
      testProperty "buy-buy-buy-sell4" $
        property $ do
          b <- forAll $ genAnnotated genLot
          let b4 = b & item . amount *~ 4
          checkJournal
            id
            do
              buy b
              buy b
              buy b
              sell b4
            do
              buy b
              open 1 Long b
              buy b
              open 2 Long b
              buy b
              open 3 Long b
              sell b4
              close 1 b
              close 2 b
              close 3 b
              open 4 Short b
            do
              open 4 Short b,
      --
      testProperty "buy2-sell" $
        property $ do
          b <- forAll $ genAnnotated genLot
          let b2 = b & item . amount *~ 2
          checkJournal
            id
            do
              buy b2
              sell b
            do
              buy b2
              open 1 Long b2
              sell b
              close 1 b
            do
              open 1 Long b,
      --
      testProperty "buy2-sell-sell" $
        property $ do
          b <- forAll $ genAnnotated genLot
          let b2 = b & item . amount *~ 2
          checkJournal
            id
            do
              buy b2
              sell b
              sell b
            do
              buy b2
              open 1 Long b2
              sell b
              close 1 b
              sell b
              close 1 b
            do
              pure (),
      --
      testProperty "sell-sell-sell" $
        property $ do
          s <- forAll $ genAnnotated genLot
          checkJournal
            id
            do
              sell s
              sell s
              sell s
            do
              sell s
              open 1 Short s
              sell s
              open 2 Short s
              sell s
              open 3 Short s
            do
              open 1 Short s
              open 2 Short s
              open 3 Short s
    ]

{-# LANGUAGE TypeApplications #-}

module Closings where

import Control.Lens hiding (each)
import Hedgehog hiding (Action)
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
          checkJournal @()
            id
            ( do
                buy b
                buy b
                buy b
            )
            ( evalDSL $ do
                bought b
                open 1 Long b
                bought b
                open 2 Long b
                bought b
                open 3 Long b
            )
            ( evalDSL $ do
                open 1 Long b
                open 2 Long b
                open 3 Long b
            ),
      --
      testProperty "buy-buy-buy-sell-sell-sell" $
        property $ do
          b <- forAll $ genAnnotated genLot
          checkJournal @()
            id
            ( do
                buy b
                buy b
                buy b
                sell b
                sell b
                sell b
            )
            ( evalDSL $ do
                bought b
                open 1 Long b
                bought b
                open 2 Long b
                bought b
                open 3 Long b
                sold b
                close 1 b 0
                sold b
                close 2 b 0
                sold b
                close 3 b 0
            )
            (pure ()),
      --
      testProperty "buy-buy-buy-sell2" $
        property $ do
          b <- forAll $ genAnnotated genLot
          let b2 = b & item . amount *~ 2
          checkJournal @()
            id
            ( do
                buy b
                buy b
                buy b
                sell b2
            )
            ( evalDSL $ do
                bought b
                open 1 Long b
                bought b
                open 2 Long b
                bought b
                open 3 Long b
                sold b2
                close 1 b 0
                close 2 b 0
            )
            ( evalDSL $ do
                open 3 Long b
            ),
      --
      testProperty "buy-buy-buy-sell3" $
        property $ do
          b <- forAll $ genAnnotated genLot
          let b3 = b & item . amount *~ 3
          checkJournal @()
            id
            ( do
                buy b
                buy b
                buy b
                sell b3
            )
            ( evalDSL $ do
                bought b
                open 1 Long b
                bought b
                open 2 Long b
                bought b
                open 3 Long b
                sold b3
                close 1 b 0
                close 2 b 0
                close 3 b 0
            )
            (pure ()),
      --
      testProperty "buy-buy-buy-sell4" $
        property $ do
          b <- forAll $ genAnnotated genLot
          let b4 = b & item . amount *~ 4
          checkJournal @()
            id
            ( do
                buy b
                buy b
                buy b
                sell b4
            )
            ( evalDSL $ do
                bought b
                open 1 Long b
                bought b
                open 2 Long b
                bought b
                open 3 Long b
                sold b4
                close 1 b 0
                close 2 b 0
                close 3 b 0
                open 4 Short b
            )
            ( evalDSL $ do
                open 4 Short b
            ),
      --
      testProperty "buy2-sell" $
        property $ do
          b <- forAll $ genAnnotated genLot
          let b2 = b & item . amount *~ 2
          checkJournal @()
            id
            ( do
                buy b2
                sell b
            )
            ( evalDSL $ do
                bought b2
                open 1 Long b2
                sold b
                close 1 b 0
            )
            ( evalDSL $ do
                open 1 Long b
            ),
      --
      testProperty "buy2-sell-sell" $
        property $ do
          b <- forAll $ genAnnotated genLot
          let b2 = b & item . amount *~ 2
          checkJournal @()
            id
            ( do
                buy b2
                sell b
                sell b
            )
            ( evalDSL $ do
                bought b2
                open 1 Long b2
                sold b
                close 1 b 0
                sold b
                close 1 b 0
            )
            (pure ()),
      --
      testProperty "sell-sell-sell" $
        property $ do
          s <- forAll $ genAnnotated genLot
          checkJournal @()
            id
            ( do
                sell s
                sell s
                sell s
            )
            ( evalDSL $ do
                sold s
                open 1 Short s
                sold s
                open 2 Short s
                sold s
                open 3 Short s
            )
            ( evalDSL $ do
                open 1 Short s
                open 2 Short s
                open 3 Short s
            )
    ]

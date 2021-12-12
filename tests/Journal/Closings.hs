{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

module Closings where

import Control.Lens hiding (each)
import Hedgehog hiding (Action)
import qualified Hedgehog.Gen as Gen
import Journal.Closings
import Journal.Entry
import Journal.Types
import Test.Tasty
import Test.Tasty.Hedgehog
import TestAction

testClosings :: TestTree
testClosings =
  testGroup
    "closings"
    [ testRule "buy-buy-buy" $ \runTest b ->
        runTest
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
      testRule "buy-buy-buy-sell-sell-sell" $ \runTest b ->
        runTest
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
      testRule "buy-buy-buy-sell2" $ \runTest b -> do
        let b2 = b & item . amount *~ 2
        runTest
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
      testRule "buy-buy-buy-sell2-buy" $ \runTest b -> do
        let s = b & item . price -~ 10
            s2 = s & item . amount *~ 2
        runTest
          do
            buy b
            buy b
            buy b
            sell s2
            buy b
          do
            buy b
            open 1 Long b
            --
            buy b
            open 2 Long b
            --
            buy b
            open 3 Long b
            --
            sell s2
            close 1 s
            close 2 s
            --
            buy b
            open 4 Long b
          do
            open 3 Long b
            open 4 Long b,
      --
      testRule "buy-buy-buy-sell3" $ \runTest b -> do
        let b3 = b & item . amount *~ 3
        runTest
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
      testRule "buy-buy-buy-sell4" $ \runTest b -> do
        let b4 = b & item . amount *~ 4
        runTest
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
      testRule "buy2-sell" $ \runTest b -> do
        let b2 = b & item . amount *~ 2
        runTest
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
      testRule "buy2-sell-sell" $ \runTest b -> do
        let b2 = b & item . amount *~ 2
        runTest
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
      testRule "sell-sell-sell" $ \runTest s ->
        runTest
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

testRule ::
  String ->
  ( ( TestDSL '[Const Trade, Const Deposit, Const Income, Const Options] () ->
      TestDSL
        '[ Const PositionEvent,
           Const Trade,
           Const Deposit,
           Const Income,
           Const Options
         ]
        () ->
      TestDSL
        '[ Const PositionEvent,
           Const Trade,
           Const Deposit,
           Const Income,
           Const Options
         ]
        () ->
      PropertyT IO ()
    ) ->
    Annotated Lot ->
    PropertyT IO ()
  ) ->
  TestTree
testRule name f = testProperty name $
  property $ do
    b <-
      forAll $
        Gen.filter (\b -> (b ^. item . price) > 10) $
          genAnnotated genLot
    f (checkJournal id) b

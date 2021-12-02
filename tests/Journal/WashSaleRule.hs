{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module WashSaleRule where

import Control.Arrow
import Control.Lens
import Hedgehog hiding (Action)
import qualified Hedgehog.Gen as Gen
import Journal.Closings
import Journal.Types
import Taxes.USA.WashSaleRule
import Test.Tasty
import Test.Tasty.Hedgehog
import TestAction

testWashSaleRule :: TestTree
testWashSaleRule =
  testGroup
    "wash-sale-rule"
    [ testProperty "buy-buy-sell" $
        property $ do
          b <-
            forAll $
              Gen.filter (\b -> (b ^. item . price) > 10) $
                genAnnotated genLot
          checkJournal @[Washing]
            ((id &&& openPositions FIFO) . washSaleRule . fst)
            ( do
                buy b
                buy b
                buy b
                sell $ b & item . price -~ 10
            )
            ( do
                bought b
                open 1 Long b
                bought b
                open 2 Long b
                bought b
                openWashed
                  3
                  Long
                  b
                  ((b ^. item . price) + 10)
                  [WashedFromFuture (-10) ((b ^. item) & price -~ 10)]
                sold $ b & item . price -~ 10
                closeWashed 1 (b & item . price -~ 10) (-10) [WashPast (-10) 3]
            )
            ( do
                open 2 Long b
                openWashed
                  3
                  Long
                  b
                  ((b ^. item . price) + 10)
                  [WashedFromFuture (-10) ((b ^. item) & price -~ 10)]
            )
    ]

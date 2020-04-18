{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Wash (testWashSaleRule) where

import Control.Lens
import Control.Monad.State
-- import Data.Amount
import Data.Ledger
import Test.Tasty
import Test.Tasty.HUnit
-- import ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import ThinkOrSwim.Types
import ThinkOrSwim.Wash

testWashSaleRule :: TestTree
testWashSaleRule = testGroup "washSaleRule"
    []
{-
    [ testCase "AAPL [] <-- [12@@300 $$ 0.0]" $
      runState (washSaleRule "AAPL" [(True, 12@@300 ## "2020-02-15" $$$ 0.00)])
               newGainsKeeperState
          @?= ([(True, 12.00 @@ 300.00 ## "2020-02-15" $$$ 0.00)],
               GainsKeeperState mempty
                   (mempty & at "AAPL"
                        ?~ [TransactionEvent
                                Nothing
                                (fromIso8601 "2020-02-15")
                                (12.00 @@ 300.00 ## "2020-02-15")]))
    ]
-}

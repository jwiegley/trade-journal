{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Wash (testWashSaleRule) where

import Control.Lens
import Control.Monad.State
import Data.Ledger
-- import Data.Maybe (fromJust)
import Data.Text (Text)
-- import Data.Time.Format.ISO8601
import Test.Tasty
import Test.Tasty.HUnit
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import ThinkOrSwim.Types
import ThinkOrSwim.Wash

testWashSaleRule :: TestTree
testWashSaleRule = testGroup "washSaleRule"
    [ testCase "opening transaction, empty history" $ do
      let aapl0215 = 12@@300 ## "2020-02-15" $$$ 0.00
      wash "AAPL" [ aapl0215 ] []
          @?= ( [ aapl0215 ]
              , [ openEvent aapl0215 ])
    ]

openEvent
    :: LotAndPL API.TransactionSubType API.Transaction
    -> TransactionEvent API.TransactionSubType API.Transaction
openEvent l = TransactionEvent
    Nothing (l^?!plLot.purchaseDate._Just) (l^.plLot)

wash :: Text
     -> [LotAndPL API.TransactionSubType API.Transaction]
     -> [TransactionEvent API.TransactionSubType API.Transaction]
     -> ([LotAndPL API.TransactionSubType API.Transaction],
        [TransactionEvent API.TransactionSubType API.Transaction])
wash sym pls evs =
    fixup $ runState (washSaleRule sym (map (True,) pls)) st
  where
    st = newGainsKeeperState & positionEvents.at sym ?~ evs

    fixup ((_, pls'), st') = (map snd pls', st'^?!positionEvents.ix sym)

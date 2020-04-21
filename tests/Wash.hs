{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Wash (testWashSaleRule) where

import Control.Lens
import Control.Monad.Morph
import Control.Monad.Trans.State
import Data.Ledger
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Text.PrettyPrint
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import ThinkOrSwim.Types
import ThinkOrSwim.Wash

testWashSaleRule :: TestTree
testWashSaleRule = testGroup "washSaleRule"
    [ testCase "open" $
      flip evalStateT newGainsKeeperState $ do
          let aapl0215 = 12@@300 ## "2020-02-15" $$$ 0.00
          res <- wash "AAPL" [ aapl0215 ]
          lift $ res @?= [ aapl0215 ]

    , testCase "open, sell at loss <30, open again <30" $
      flip evalStateT newGainsKeeperState $ do
          let aapl0215o =    12@@300 ## "2020-02-15" $$$     0.00
                  & plLot.refs .~ [ openRef ]
              aapl0215c = (-12)@@290 ## "2020-02-16" $$$   120.00
                  & plKind     .~ LossShort
                  & plLot.refs .~ [ openRef ]
              aapl0215w =    12@@420 ## "2020-02-15" $$$ (-120.00)
                  & plKind     .~ WashLoss
                  & plLot.refs .~
                      [ openRef, Ref (WashSaleRule 120.00) 1 Nothing ]

          wash "AAPL" [ aapl0215o ] @?== [ aapl0215o ]
          wash "AAPL" [ aapl0215c ] @?== [ aapl0215c ]
          wash "AAPL" [ aapl0215o ] @?== [ aapl0215w ]

    , testCase "open, open, sell at loss <30" $
      flip evalStateT newGainsKeeperState $ do
          let aapl0215o1 = 12@@300 ## "2020-02-15" $$$ 0.00
                  & plLot.refs .~ [ openRef & refId .~ 1 ]
              aapl0215o2 = 10@@310 ## "2020-02-16" $$$ 0.00
                  & plLot.refs .~ [ openRef & refId .~ 2 ]
              aapl0215c1a = (-12)@@290 ## "2020-02-17" $$$ 120.00
                  & plKind     .~ LossShort
                  & plLot.refs .~ [ openRef & refId .~ 1 ]
              aapl0215c2a = (-10)@@241.6667 ## "2020-02-17" $$$ 0.00
                  & plKind     .~ LossShort
                  & plLot.refs .~ [ openRef & refId .~ 1 ]
              aapl0215c2w = 10@@410 ## "2020-02-16" $$$ (-100.00)
                  & plKind     .~ WashLoss
                  & plLot.refs .~
                      [ openRef & refId .~ 2
                      , Ref (WashSaleRule 100.00) 1 Nothing ]
              aapl0215c1b = (-2)@@48.3333 ## "2020-02-17" $$$ 20.00
                  & plKind     .~ LossShort
                  & plLot.refs .~ [ openRef & refId .~ 1 ]

          wash "AAPL" [ aapl0215o1  ] @?== [ aapl0215o1 ]
          wash "AAPL" [ aapl0215o2  ] @?== [ aapl0215o2 ]
          wash "AAPL" [ aapl0215c1a ] @?== [ aapl0215c2a, aapl0215c2w
                                           , aapl0215c1b ]

    -- , testCase "open, sell at loss >30, open <30" $ do
    -- , testCase "open, sell at loss <30, open >30" $ do
    -- , testCase "open, open, sell at loss >30" $ do
    -- , testCase "open, open, sell at loss <30, sell at loss <30, open" $ do
    ]

openRef :: Ref API.Transaction
openRef = Ref OpeningOrder 1 Nothing

(@?==) :: (Show a, Eq a) => StateT s IO a -> a -> StateT s IO ()
action @?== result = do
    res <- action
    lift $ res @?= result

wash :: Text
     -> [LotAndPL API.TransactionSubType API.Transaction]
     -> StateT (GainsKeeperState API.TransactionSubType API.Transaction)
           IO [LotAndPL API.TransactionSubType API.Transaction]
wash sym pls = do
    (d, res) <- hoist (pure . runIdentity)
                     (washSaleRule sym ((True,) <$> pls))
    lift $ putStrLn $ render d
    pure $ map snd res

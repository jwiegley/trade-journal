{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Wash (testWashSaleRule) where

import Control.Lens
import Control.Monad
import Control.Monad.Morph
import Control.Monad.Trans.State
import Data.Ledger
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Show.Pretty
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API hiding (cost)
import ThinkOrSwim.Gains
import ThinkOrSwim.Types
import ThinkOrSwim.Wash

testWashSaleRule :: TestTree
testWashSaleRule = testGroup "washSaleRule"
    [ testCase "open" $
      flip evalStateT [] $ do
          let aapl0215 = 12@@300 ## "2020-02-15" $$$ 0.00
          wash "AAPL" aapl0215 @?==
              ( [ aapl0215 ]
              , [ aapl0215 ]
              )

    , testCase "open, open <30" $
      flip evalStateT [] $ do
          let aapl0215 = 12@@300 ## "2020-02-15" $$$ 0.00
              aapl0216 = 12@@300 ## "2020-02-16" $$$ 0.00
          wash "AAPL" aapl0215
          wash "AAPL" aapl0216 @?==
              ( [ aapl0215
                , aapl0216 ]
              , [ aapl0216 ]
              )

    , testCase "open, open >30" $
      flip evalStateT [] $ do
          let aapl0215 = 12@@300 ## "2020-02-15" $$$ 0.00
              aapl0416 = 12@@300 ## "2020-04-16" $$$ 0.00
          wash "AAPL" aapl0215
          wash "AAPL" aapl0416 @?==
              ( [ aapl0416 ]
              , [ aapl0416 ]
              )

    , testCase "open, sell at loss <30" $
      flip evalStateT [] $ do
          let aapl0215 =    12@@300 ## "2020-02-15" $$$    0.00
              aapl0216 = (-12)@@200 ## "2020-02-16" $$$ 1200.00
          wash "AAPL" aapl0215
          wash "AAPL" aapl0216 @?==
              ( [ aapl0216 ]
              , [ aapl0216 ]
              )

    , testCase "open, sell at gain" $
      flip evalStateT [] $ do
          let aapl0215 =    12@@300 ## "2020-02-15" $$$      0.00
              aapl0216 = (-12)@@400 ## "2020-02-16" $$$ (-1200.00)
          wash "AAPL" aapl0215
          wash "AAPL" aapl0216 @?==
              ( [ aapl0215 ]
              , [ aapl0216 ]
              )

    , testCase "open, sell at loss >30" $
      flip evalStateT [] $ do
          let aapl0215 =    12@@300 ## "2020-02-15" $$$    0.00
              aapl0416 = (-12)@@200 ## "2020-04-16" $$$ 1200.00
          wash "AAPL" aapl0215
          wash "AAPL" aapl0416 @?==
              ( [ ]
              , [ aapl0416 ]
              )

    , testCase "open, sell at loss <30, open again <30" $
      flip evalStateT [] $ do
          let aapl0215o =    12@@300 ## "2020-02-15" $$$     0.00
                  & plLot.refs .~ [ openRef & refId .~ 1 ]
              aapl0216c = (-12)@@290 ## "2020-02-16" $$$   120.00
                  & plKind     .~ LossShort
                  & plLot.refs .~ [ openRef & refId .~ 1 ]
              aapl0217o =    12@@300 ## "2020-02-17" $$$     0.00
                  & plLot.refs .~ [ openRef & refId .~ 3 ]

          wash "AAPL" aapl0215o @?==
              ( [ aapl0215o ]
              , [ aapl0215o ]
              )
          wash "AAPL" aapl0216c @?==
              ( [ aapl0216c ]
              , [ aapl0216c ]
              )
          wash "AAPL"
              aapl0217o
              @?==
              ( [ 12@@420 ## "2020-02-17" $$$ 0.00
                      & plLot.refs .~
                          [ openRef & refId .~ 3
                          , Ref (WashSaleRule 120.00) 1 Nothing ]
                ]
              , [ 12@@420 ## "2020-02-17" $$$ (-120.00)
                      & plKind      .~ WashLoss
                      & plLot.refs  .~
                          [ openRef & refId .~ 3
                          , Ref (WashSaleRule 120.00) 1 Nothing ]
                ]
              )

{-
    , testCase "open, open, sell at loss <30" $
      flip evalStateT [] $ do
          let aapl0215o1  =    12@@300 ## "2020-02-15" $$$   0.00
                  & plLot.refs .~ [ openRef & refId .~ 1 ]
              aapl0216o2  =    10@@310 ## "2020-02-16" $$$   0.00
                  & plLot.refs .~ [ openRef & refId .~ 2 ]
              aapl0217c1a = (-12)@@290 ## "2020-02-17" $$$ 120.00
                  & plKind     .~ LossShort
                  & plLot.refs .~ [ openRef & refId .~ 1 ]

          wash "AAPL" [ aapl0215o1  ] @?==
              ( [ aapl0215o1 ]
              , [ aapl0215o1 ]
              )
          wash "AAPL" [ aapl0216o2  ] @?==
              ( [ aapl0216o2 ]
              , [ aapl0216o2 ]
              )
          wash "AAPL" [ aapl0217c1a ] @?==
              ( [ aapl0217c1a
                      & plLoss .~ 20.00
                , (-10)@@310      ## "2020-02-16" $$$     0.00
                      & plKind     .~ LossShort
                      & plLot.refs .~ [ openRef & refId .~ 2 ]
                , aapl0216o2
                      & plKind     .~ WashLoss
                      & plLoss     .~ (-100.00)
                      & plLot.cost ?~ 410.00
                      & plLot.refs .~
                          [ openRef & refId .~ 2
                          , Ref (WashSaleRule 100.00) 1 Nothing ] ]
              , [ aapl0217c1a
                      & plLoss .~ 20.00
                , (-10)@@310      ## "2020-02-16" $$$     0.00
                      & plKind     .~ LossShort
                      & plLot.refs .~ [ openRef & refId .~ 2 ]
                , aapl0216o2
                      & plKind     .~ WashLoss
                      & plLoss     .~ (-100.00)
                      & plLot.cost ?~ 410
                      & plLot.refs .~
                          [ openRef & refId .~ 2
                          , Ref (WashSaleRule 100.00) 1 Nothing ] ]
              )
-}
    ]

openRef :: Ref API.Transaction
openRef = Ref OpeningOrder 1 Nothing

assertEqual'
  :: (Eq a, Show a, HasCallStack)
  => String -- ^ The message prefix
  -> a      -- ^ The expected value
  -> a      -- ^ The actual value
  -> Assertion
assertEqual' preface expected actual =
  unless (actual == expected) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ ppShow expected ++ "\n but got: " ++ ppShow actual

(@?==) :: (Show a, Eq a) => StateT s IO a -> a -> StateT s IO ()
action @?== result = do
    res <- action
    lift $ assertEqual' "" result res

wash :: Text
     -> LotAndPL API.TransactionSubType API.Transaction
     -> StateT [LotAndPL API.TransactionSubType API.Transaction]
           IO ( [LotAndPL API.TransactionSubType API.Transaction]
              , [LotAndPL API.TransactionSubType API.Transaction] )
wash sym pls = hoist (pure . runIdentity) $ do
    res <- washSaleRule pls
    events <- get
    pure (events, res)

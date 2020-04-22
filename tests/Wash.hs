{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Wash (testWashSaleRule) where

import Control.Arrow ((***))
import Control.Lens
import Control.Monad
import Control.Monad.Morph
import Control.Monad.Trans.State
import Data.Ledger as L
import Test.Tasty
import Test.Tasty.HUnit
import Text.Show.Pretty
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import ThinkOrSwim.Transaction ()
import ThinkOrSwim.Wash

testWashSaleRule :: TestTree
testWashSaleRule = testGroup "washSaleRule"
    [ testCase "open" $
      flip evalStateT [] $ do
          let aapl0215 = 12@@300 ## "2020-02-15" $$$ 0.00
          wash aapl0215 @?==
              ( [ aapl0215 ]
              , [ aapl0215 ]
              )

    , testCase "open, open <30" $
      flip evalStateT [] $ do
          let aapl0215 = 12@@300 ## "2020-02-15" $$$ 0.00
              aapl0216 = 12@@300 ## "2020-02-16" $$$ 0.00
          wash aapl0215
          wash aapl0216 @?==
              ( [ aapl0215
                , aapl0216 ]
              , [ aapl0216 ]
              )

    , testCase "open, open >30" $
      flip evalStateT [] $ do
          let aapl0215 = 12@@300 ## "2020-02-15" $$$ 0.00
              aapl0416 = 12@@300 ## "2020-04-16" $$$ 0.00
          wash aapl0215
          wash aapl0416 @?==
              ( [ aapl0416 ]
              , [ aapl0416 ]
              )

    , testCase "open, sell at loss <30" $
      flip evalStateT [] $ do
          let aapl0215 =    12@@300 ## "2020-02-15" $$$    0.00
              aapl0216 = (-12)@@200 ## "2020-02-16" $$$ 1200.00
          wash aapl0215
          wash aapl0216 @?==
              ( [ aapl0216 ]
              , [ aapl0216 ]
              )

    , testCase "open, sell at gain" $
      flip evalStateT [] $ do
          let aapl0215 =    12@@300 ## "2020-02-15" $$$      0.00
              aapl0216 = (-12)@@400 ## "2020-02-16" $$$ (-1200.00)
          wash aapl0215
          wash aapl0216 @?==
              ( [ aapl0215 ]
              , [ aapl0216 ]
              )

    , testCase "open, sell at loss >30" $
      flip evalStateT [] $ do
          let aapl0215 =    12@@300 ## "2020-02-15" $$$    0.00
              aapl0416 = (-12)@@200 ## "2020-04-16" $$$ 1200.00
          wash aapl0215
          wash aapl0416 @?==
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

          wash aapl0215o @?==
              ( [ aapl0215o ]
              , [ aapl0215o ]
              )
          wash aapl0216c @?==
              ( [ aapl0216c ]
              , [ aapl0216c ]
              )
          wash aapl0217o
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

    , testCase "open, open, sell at loss <30" $
      flip evalStateT [] $ do
          let aapl0215o  =    12@@300 ## "2020-02-15" $$$   0.00
                  & plLot.refs .~ [ openRef & refId .~ 1 ]
              aapl0216o  =    10@@310 ## "2020-02-16" $$$   0.00
                  & plLot.refs .~ [ openRef & refId .~ 2 ]
              aapl0217c = (-12)@@290 ## "2020-02-17" $$$ 120.00
                  & plKind .~ LossShort
                  & plLot.refs .~ [ openRef & refId .~ 1 ]

          wash aapl0215o @?==
              ( [ aapl0215o ]
              , [ aapl0215o ]
              )
          wash aapl0216o @?==
              ( [ aapl0215o
                , aapl0216o ]
              , [ aapl0216o ]
              )
          wash aapl0217c @?==
              ( [ aapl0216o
                    & plKind .~ BreakEven
                    & plLoss .~ 0
                    & plLot.L.cost ?~ 410.00
                    & plLot.refs <>~
                        [ Ref (WashSaleRule 100.00) 1 Nothing ]
                , (-2)@@48.3333 ## "2020-02-17" $$$ 20.00
                    & plKind .~ LossShort
                    & plLot.refs .~ [ openRef & refId .~ 1 ] ]

              , [ aapl0217c
                , aapl0216o
                    & plKind .~ BreakEven
                    & plLoss .~ 0.00
                    & plLot.L.quantity %~ negate
                , aapl0216o
                    & plKind .~ WashLoss
                    & plLoss .~ (-100.00)
                    & plLot.L.cost ?~ 410
                    & plLot.refs .~
                        [ openRef & refId .~ 2
                        , Ref (WashSaleRule 100.00) 2 Nothing ] ]
              )
    ]

openRef :: Ref API.Transaction
openRef = Ref OpeningOrder 1 Nothing

assertEqual'
  :: (Eq k, Show k, Eq t, Show t, HasCallStack)
  => String -- ^ The message prefix
  -> ([LotAndPL k t], [LotAndPL k t]) -- ^ The expected value
  -> ([LotAndPL k t], [LotAndPL k t]) -- ^ The actual value
  -> Assertion
assertEqual' preface expected actual =
    unless (actual == expected) $ do
        putStrLn $ msg (ppShow . (map showLotAndPL *** map showLotAndPL))
        assertFailure (msg ppShow)
  where
    msg f = (if null preface then "" else preface ++ "\n")
       ++ "expected: " ++ f expected
       ++ "\n but got: " ++ f actual

(@?==) :: (Eq k, Show k, Eq t, Show t)
       => StateT s IO ([LotAndPL k t], [LotAndPL k t])
       -> ([LotAndPL k t], [LotAndPL k t])
       -> StateT s IO ()
action @?== result = do
    res <- action
    lift $ assertEqual' "" result res

wash :: LotAndPL API.TransactionSubType API.Transaction
     -> StateT [LotAndPL API.TransactionSubType API.Transaction]
           IO ( [LotAndPL API.TransactionSubType API.Transaction]
              , [LotAndPL API.TransactionSubType API.Transaction] )
wash pls = hoist (pure . runIdentity) $ do
    res <- washSaleRule pls
    events <- get
    pure (events, res)

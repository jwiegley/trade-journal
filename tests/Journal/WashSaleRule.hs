{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module WashSaleRule where

import Amount
import Control.Arrow
import Control.Lens
import Control.Monad.State
import Data.Map (Map)
import Data.Text (Text)
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
          checkJournal
            washSaleRuleTest
            do
              buy b
              buy b
              buy b
              sell $ b & item . price -~ 10
            do
              bought b
              open 1 Long b
              --
              bought b
              open 2 Long b
              --
              bought b
              washedFrom Future (-10) $
                open 3 Long b
              --
              sold $ b & item . price -~ 10
              wash Past 3 $
                close 1 b (-10)
            do
              open 2 Long b
              washedFrom Future (-10) $
                open 3 Long b,
      --
      testProperty "buy-sell-buy" $
        property $ do
          b <-
            forAll $
              Gen.filter (\b -> (b ^. item . price) > 10) $
                genAnnotated genLot
          checkJournal
            washSaleRuleTest
            do
              buy b
              sell $ b & item . price -~ 10
              buy b
              buy b
            do
              bought b
              open 1 Long b
              --
              sold $ b & item . price -~ 10
              wash Future 2 $
                close 1 b (-10)
              --
              bought b
              washedFrom Past (-10) $
                open 2 Long b
              --
              bought b
              open 3 Long b
            do
              washedFrom Past (-10) $
                open 2 Long b
              open 3 Long b,
      --
      testProperty "buy-buy-sell-buy-sell" $
        property $
          do
            b <-
              forAll $
                Gen.filter (\b -> (b ^. item . price) > 10) $
                  genAnnotated genLot
            checkJournal
              washSaleRuleTest
              do
                buy b
                buy b
                sell b
                buy b
                sell $ b & item . price -~ 10
              do
                bought b
                open 1 Long b
                --
                bought b
                open 2 Long b
                --
                sold b
                close 1 b 0
                --
                bought b
                washedFrom Future (-10) $
                  open 3 Long b
                --
                sold $ b & item . price -~ 10
                wash Past 3 $
                  close 2 b (-10)
              do
                washedFrom Future (-10) $
                  open 3 Long b,
      --
      testProperty
        "buy-buy-sell-sell"
        $ property $ do
          b <-
            forAll $
              Gen.filter (\b -> (b ^. item . price) > 10) $
                genAnnotated genLot
          checkJournal
            washSaleRuleTest
            do
              buy b
              buy b
              let s = b & item . price -~ 10
              sell s
              sell s
            do
              bought b
              open 1 Long b
              --
              bought b
              washedFrom Future (-10) $
                open 2 Long b
              --
              let s = b & item . price -~ 10
              -- sold s
              -- wash Past 2 $
              --   close 1 s 0
              sold s
              wash Past 2 $
                close 1 b (-10)
              --
              sold s
              close 2 s (-20)
            do
              pure mempty
    ]

washSaleRuleTest ::
  ( ([Annotated (Entry [Washing])], Map Text [Annotated (Entry [Washing])]) ->
    ([Annotated (Entry [Washing])], Map Text [Annotated (Entry [Washing])])
  )
washSaleRuleTest = (id &&& openPositions FIFO) . washSaleRule . fst

washedFrom ::
  Period ->
  Amount 6 ->
  TestDSL [Washing] () ->
  TestDSL [Washing] ()
washedFrom period loss act = do
  let o = head $ execState act []
  case o of
    EOpen p@(view item -> pos) ->
      id
        <>= [ EOpen
                ( ( pos
                      & posBasis -~ loss
                      & posData
                        <>~ [ WashedFrom
                                period
                                loss
                                (pos ^. posLot & price +~ loss)
                            ]
                  )
                    <$ p
                )
            ]
    _ ->
      error $ "washFrom can only be used on opening event: " ++ show o

wash ::
  Period ->
  Int ->
  TestDSL [Washing] () ->
  TestDSL [Washing] ()
wash period n act = do
  let c = head $ execState act []
  case c of
    EClose i l loss [] ->
      id <>= [EClose i (l & item . price +~ loss) loss [Wash period loss n]]
    _ ->
      error $ "washFrom can only be used on opening event: " ++ show c

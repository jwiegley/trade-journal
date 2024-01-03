{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module WashSaleRule where

import Amount
import Control.Arrow
import Control.Lens hiding (Context)
import Control.Monad.State
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Data.Time.Format
import Hedgehog hiding (Action)
import qualified Hedgehog.Gen as Gen
import Journal.Closings
import Journal.Entry
import Journal.Parse
import Journal.Types
import Taxes.USA.WashSaleRule
import Test.Tasty
import Test.Tasty.Hedgehog
import TestAction hiding (positions)

testWashSaleRule :: TestTree
testWashSaleRule =
  testGroup
    "wash-sale-rule"
    [ testRule "buy-buy-sell" $ \runTest b -> do
        let s = b & item . price -~ 10
        runTest
          do
            buy b
            buy b
            buy b
            sell s
          do
            buy b
            open 1 Long b
            --
            buy b
            open 2 Long b
            --
            buy b
            washedFrom Future 1 s (-10) $
              open 3 Long b
            --
            sell s
            wash Past 3 b (-10) $
              close 1 s
          do
            open 2 Long b
            washedFrom Future 1 s (-10) $
              open 3 Long b,
      --
      testRule "buy-sell-buy" $ \runTest b -> do
        let s = b & item . price -~ 10
        runTest
          do
            buy b
            sell s
            buy b
            buy b
          do
            buy b
            open 1 Long b
            --
            sell s
            wash Future 2 b (-10) $
              close 1 s
            --
            buy b
            washedFrom Past 1 s (-10) $
              open 2 Long b
            --
            buy b
            open 3 Long b
          do
            washedFrom Past 1 s (-10) $
              open 2 Long b
            open 3 Long b,
      --
      testRule "buy-buy-sell-buy-sell" $ \runTest b -> do
        let s = b & item . price -~ 10
        runTest
          do
            buy b
            buy b
            sell b
            buy b
            sell s
          do
            buy b
            open 1 Long b
            --
            buy b
            open 2 Long b
            --
            sell b
            close 1 b
            --
            buy b
            washedFrom Future 2 s (-10) $
              open 3 Long b
            --
            sell s
            wash Past 3 b (-10) $
              close 2 s
          do
            washedFrom Future 2 s (-10) $
              open 3 Long b,
      --
      testRule "buy-buy2-sell-sell" $ \runTest b -> do
        let b2 = b & item . amount *~ 2
        let s = b & item . price -~ 10
        runTest
          do
            buy b
            buy b2
            sell s
            sell b2
          do
            buy b
            open 1 Long b
            --
            buy b2
            washedFrom Future 1 s (-10) $
              open 2 Long b2
            --
            sell s
            wash Past 2 b2 (-10) $
              close 1 s
            --
            sell b2
            close 2 b2
          do
            pure (),
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
            washedFrom Future 1 s (-10) $
              open 3 Long b
            --
            sell s2
            wash Past 3 b (-10) $
              close 1 s
            wash Future 4 b (-10) $
              close 2 s
            --
            buy b
            washedFrom Past 2 s (-10) $
              open 4 Long b
          do
            washedFrom Future 1 s (-10) $
              open 3 Long b
            washedFrom Past 2 s (-10) $
              open 4 Long b,
      --
      {- jww (2021-12-11): What should happen in this case?

         1. Do we wash half of the losing s2 sale into the remainder of 1 and
            then into 2?
         2. Do we just wash half into 2?
         2. Do we not wash at all until there's opening large enough?

            testRule "buy3-sell2-sell-buy" $ \runTest b -> do
              let b3 = b & item . amount *~ 3
              let s2 =
                    b & item . amount *~ 2
                      & item . price -~ 10
              runTest
                do
                  buy b3
                  sell s2
                  sell b
                  buy b
                do
                  buy b3
                  open 1 Long b3
                  --
                  sell s2
                  wash Future 2 b (-10) $
                    close 1 s2
                  --
                  sell b
                  close 1 b
                  --
                  buy b
                  washedFrom Past 1 s2 (-10) $
                    open 2 Long b
                do
                  washedFrom Past 1 s2 (-10) $
                    open 2 Long b,
      -}
      --
      testRule "buy-buy-sell-sell" $ \runTest b -> do
        let s = b & item . price -~ 10
        runTest
          do
            buy b
            buy b
            sell s
            sell s
          do
            buy b
            open 1 Long b
            --
            buy b
            washedFrom Future 1 s (-10) $
              open 2 Long b
            --
            sell s
            wash Past 2 b (-10) $
              close 1 s
            --
            sell s
            close 2 s
          -- jww (2021-12-07): Implement a balance checker:
          -- balance AMOUNT
          do
            pure ()
            --
            -- testRule "complicated-buys-and-sells" $ \runTest _b -> do
            --   runTest
            --     do
            --       event "2019-06-24 transfer 140 ZM 99.7792 from ext"
            --     do
            --       pure ()
            --     do
            --       pure ()
    ]

event ::
  forall r.
  ( Populate Parser r,
    '[ Const Deposit,
       Const Income,
       Const Options,
       Const Trade
     ]
      :<: r
  ) =>
  Text ->
  TestDSL r ()
event txt = do
  case parseEntriesFromText @Maybe @r "" txt of
    Nothing -> error $ "event could not be parsed: " ++ TL.unpack txt
    Just xs -> put $ map (_Entity #) xs

on ::
  '[ Const Deposit,
     Const Income,
     Const Options,
     Const Trade
   ]
    :<: r =>
  String ->
  TestDSL r () ->
  TestDSL r ()
on date (flip execState [] -> [e]) =
  put
    [ e & _Entity . time
        .~ parseTimeOrError False defaultTimeLocale "%m/%d" date
    ]
on _ _ = error "Incorrect use of on"

testRule ::
  String ->
  ( ( TestDSL '[Const Trade, Const Deposit, Const Income, Const Options] () ->
      TestDSL
        '[ Const Washing,
           Const PositionEvent,
           Const Trade,
           Const Deposit,
           Const Income,
           Const Options
         ]
        () ->
      TestDSL
        '[ Const Washing,
           Const PositionEvent,
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
    f (checkJournal washSaleRuleTest) b
  where
    washSaleRuleTest = (id &&& positions) . washSaleRule . fst

wash ::
  Period ->
  Int ->
  Annotated Lot ->
  Amount 6 ->
  TestDSL
    '[ Const PositionEvent,
       Const Trade,
       Const Deposit,
       Const Income,
       Const Options
     ]
    () ->
  TestDSL
    '[ Const Washing,
       Const PositionEvent,
       Const Trade,
       Const Deposit,
       Const Income,
       Const Options
     ]
    ()
wash
  period
  n
  lot
  pl
  (flip execState [] -> [APositionEvent c@(view item -> Close _cl)]) =
    id
      <>= [ APositionEvent c,
            AWashing
              ( WashTo
                  Washed
                    { _washedPeriod = period,
                      _washedPos = n,
                      _washedAmount = lot ^. item . amount,
                      _washedAdjust = pl
                    }
                  <$ c
              )
          ]
wash _ _ _ _ _ = error "Incorrect use of wash"

washedFrom ::
  Period ->
  Int ->
  Annotated Lot ->
  Amount 6 ->
  TestDSL
    '[ Const PositionEvent,
       Const Trade,
       Const Deposit,
       Const Income,
       Const Options
     ]
    () ->
  TestDSL
    '[ Const Washing,
       Const PositionEvent,
       Const Trade,
       Const Deposit,
       Const Income,
       Const Options
     ]
    ()
washedFrom
  period
  n
  lot
  loss
  (flip execState [] -> [APositionEvent o@(view item -> Open pos)]) =
    id
      <>= [ APositionEvent o,
            AWashing
              ( let proratedLoss =
                      ( (loss * lot ^. item . amount)
                          / (pos ^. posLot . amount)
                      )
                 in WashFrom
                      Washed
                        { _washedPeriod = period,
                          _washedPos = n,
                          _washedAmount = lot ^. item . amount,
                          _washedAdjust = proratedLoss
                        }
                      <$ o
              )
          ]
washedFrom _ _ _ _ _ = error "Incorrect use of washedFrom"

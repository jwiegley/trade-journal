{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Gains (testGainsKeeper) where

import Control.Lens
import Data.Amount
import Data.Coerce
import Data.Split
import Pos
import Test.Tasty
import Test.Tasty.HUnit (testCase)
import ThinkOrSwim.Gains
import ThinkOrSwim.Transaction

pl :: Transactional a => Amount 4 -> a -> a -> Amount 2
pl q x y = coerce $ q * (y^.cost / abs (y^.quantity) -
                         x^.cost / abs (x^.quantity))

testGainsKeeper :: TestTree
testGainsKeeper = testGroup "gainsKeeper"
    [ testCase "closeLot 1" $
      q12c300 `closeLot` qn10c500
          @?= Applied
                (pl (-10) q12c300 qn10c500)
                (Some ((-10)@@(10 * (300/12)))
                      (2@@(2 * (300/12))))
                (All qn10c500)

    , testCase "closeLot 2" $
      q10c500 `closeLot` qn12c300
          @?= Applied
                (pl (-10) qn10c500 q12c300)
                (All ((-10)@@(10 * (500/10))))
                (Some ((-10)@@(10 * (300/12)))
                      ((-2)@@(2 * (300/12))))

    , testCase "closeLot 3" $
      q12c500 `closeLot` qn10c300
          @?= Applied
                (pl (-10) q12c500 qn10c300)
                (Some ((-10)@@(10 * (500/12)))
                      (2@@(2 * (500/12))))
                (All qn10c300)

    , testCase "closeLot 4" $
      q10c300 `closeLot` qn12c500
          @?= Applied
                (pl (-10) qn10c300 q12c500)
                (All ((-10)@@(10 * (300/10))))
                (Some ((-10)@@(10 * (500/12)))
                      ((-2)@@83.33333333333333))

    , testCase "closeLot 5" $
      qn10c300 `closeLot` q12c500
          @?= Applied
                (pl 10 qn10c300 q12c500)
                (All (10@@(10 * (300/10))))
                (Some (10@@(10 * (500/12)))
                      (2@@(2 * (500/12))))

    , testCase "closeLot 6" $
      qn10c500 `closeLot` q12c300
          @?= Applied
                (pl 10 qn10c500 q12c300)
                (All (10@@(10 * (500/10))))
                (Some (10@@(10 * (300/12)))
                      (2@@(2 * (300/12))))

    , testCase "consider closeLot -400" $
      consider closeLot (const (set loss))
          [ 100 @@ 17350.00
          , 400 @@ 69722.60
          ]
          ((-400) @@ 69727.28)
          @?= Considered
                  [ (-100) @@ 17350.00 $$ (-81.82)
                  , (-300) @@ 52291.95000000001 $$ (-3.51) ]
                  [ 100 @@ 17430.65 ]
                  [ (-100) @@ 17431.82
                  , (-300) @@ 52295.46 ]
                  Nothing

    , testCase "consider closeLot 400" $
      consider closeLot (const (set loss))
          [ 100 @@ 17350.00
          ]
          (400 @@ 69722.60)
          @?= Considered
                  []
                  [ 100 @@ 17350.00 ]
                  []
                  (Just (400 @@ 69722.60))

    , testCase "consider closeLot SNAP" $
      consider closeLot (const (set loss))
          [ 700.0 @@ 12053.72
          , 300.0 @@ 5165.97
          ]
          ((-11.0) @@ 189.97)
      @?= Considered
              [ (-11) @@ 189.41559999999998 $$ (-0.5544) ]
              [ 689.0 @@ 11864.304399999999
              , 300.0 @@ 5165.97 ]
              [ (-11) @@ 189.97 ]
              Nothing

    , testCase "handleFees opening position" $
      handleFees
          0.81 [ 100 @@ 1000.00 $$ 0.0 ]
          @?= [ 100 @@ 1000.81 $$ 0.0 ]

    , testCase "handleFees closing single position" $
      handleFees
          0.81 [ (-100) @@ 1000.81 $$ 199.19 ]
          @?= [ (-100) @@ 1000.81 $$ 200.00 ]

    , testCase "handleFees closing multiple positions 1" $
      handleFees
          0.81 [ (-100) @@ 1000.81 $$ (-100.00)
               , (-100) @@ 1000.81 $$ (-100.00)
               ]
          @?= [ (-100) @@ 1000.81 $$ (-100.00 + 0.40)
              , (-100) @@ 1000.81 $$ (-100.00 + 0.40)
              ]

    , testCase "handleFees closing multiple positions 2" $
      handleFees
          0.83 [ (-10.00) @@ 19740.50 $$ (-100.00)
               , (-10.00) @@ 19707.90 $$ (-100.00)
               ]
          @?= [ (-10.00) @@ 19740.50 $$ (-100.00 + 0.41)
              , (-10.00) @@ 19707.90 $$ (-100.00 + 0.41)
              ]

    , testCase "handleFees closing multiple positions 3" $
      handleFees
          0.47 [ (-689.00) @@ 11864.3044 $$ (-34.87)
               , (-300.00) @@ 5165.97 $$ (-15.09)
               ]
          @?= [ (-689.00) @@ 11864.3044 $$ (-34.54)
              , (-300.00) @@ 5165.97 $$ (-14.95)
              ]

    -- , testCase "sumTransactions 1" $
    --   sumTransactions [ (-22.00) @@ (22 * 307.772887) $$  (-4.85)
    --               , (-75.00) @@ (75 * 331.241137) $$ 1743.57
    --               ,  (-3.00) @@ ( 3 * 307.745049) $$  (-0.75)
    --               ]
    --       @?= (-30799.35)       -- Ledger's answer

    -- , testCase "sumTransactions 2" $
    --   sumTransactions [ 78.00 @@ (78 * 308.5035)  $$     0.00
    --               , 22.00 @@ (22 * 326.87785) $$ (-404.24)
    --               ]
    --       @?= 30850.35

    -- , testCase "sumTransactions 3" $
    --   sumTransactions [ (-100.00) @@ (100 * 307.745049) $$ (-338.09)
    --               ,  (-78.00) @@ ( 78 * 308.5035)   $$ (-204.55)
    --               ,  (-22.00) @@ ( 22 * 326.87785)  $$   346.54
    --               ]
    --       @?= (-62225.19)

    -- , testCase "sumTransactions 4" $
    --   sumTransactions [ 144.00 @@ (144 * 311.8644) $$ (-18.02)
    --               ,  56.00 @@ ( 56 * 312.1861) $$    0.00
    --               ]
    --       @?= 62372.88

    -- , testCase "sumTransactions 5" $
    --   sumTransactions [ (-144.00) @@ (144 * 311.8644) $$ 174.87
    --               ,  (-56.00) @@ ( 56 * 312.1861) $$  86.02
    --               ]
    --       @?= (-62130.01)

    , testCase "sumTransactions 6" $
      sumTransactions
          [ 112.00 @@ (112 * 309.7473) $$ 0.00
          ,  88.00 @@ ( 88 * 309.7521) $$ (-0.42)
          ]
          @?= 61949.46

    , testCase "sumTransactions 7" $
      sumTransactions
          [ (-34.00) @@ (34 * 309.7473) $$ 11.41
          , (-78.00) @@ (78 * 309.7473) $$ 26.96
          , (-88.00) @@ (88 * 309.7521) $$ 30.84
          ]
          @?= (-61880.67)

    , testCase "sumTransactions 8" $
      sumTransactions
          [ (-90.00) @@ (90 * 99.7792) $$ 1170.41
          , (-10.00) @@ (10 * 89.785)  $$   30.10
          ]
          @?= (-8677.47)

{-
    , testCase "fixupTransaction" $
      let xact = Transaction
              { _actualDate    = fromJust (iso8601ParseM "2020-03-01")
              , _effectiveDate = Nothing
              , _code          = "TEST"
              , _payee         = "TEST"
              , _postings      =
                [ newPosting Ledger.Commissions True
                      (DollarAmount 19.99)
                , newPosting (Equities "1")     False
                      (CommodityAmount
                           ( 100.00 @@ 33019.99
                                 & instrument .~ Stock
                                 & kind       .~ API.OptionAssignment
                                 & symbol     .~ "NFLX"
                                 & price      ?~ 330.00 ))
                , newPosting (Cash "1")         False
                      (DollarAmount (-33019.99))
                , newPosting CapitalGainShort   False
                      (DollarAmount (-168.23))
                , newPosting (Options "1")      False
                      (CommodityAmount
                           ( 1.00 @@ 168.23
                                 & instrument .~ Option
                                 & kind       .~ API.OptionAssignment
                                 & symbol     .~ "NFLX_071919P330"
                                 & price      ?~ 330.00 ))
                ]
              , _xactMetadata  = mempty
              , _provenance    = ()
              }
          res = Transaction
              { _actualDate    = fromJust (iso8601ParseM "2020-03-01")
              , _effectiveDate = Nothing
              , _code          = "TEST"
              , _payee         = "TEST"
              , _postings      =
                [ newPosting Ledger.Commissions True
                      (DollarAmount 19.99)
                , newPosting (Equities "1")     False
                      (CommodityAmount
                           ( 100.00 @@ 32851.76
                                 & instrument .~ Stock
                                 & kind       .~ API.OptionAssignment
                                 & symbol     .~ "NFLX"
                                 & price      ?~ 330.00 ))
                , newPosting (Cash "1")         False
                      (DollarAmount (-33019.99))
                , newPosting (Options "1")      False
                      (CommodityAmount
                           ( 1.00 @@ 168.23
                                 & instrument .~ Option
                                 & kind       .~ API.OptionAssignment
                                 & symbol     .~ "NFLX_071919P330"
                                 & price      ?~ 330.00 ))
                ]
              , _xactMetadata  = mempty
              , _provenance    = ()
              }
      in evalState
             (fixupTransaction xact)
             (newGainsKeeperState
                  & openTransactions.at "NFLX_071919P330"
                        ?~ [ (-1.00) @@ 168.23
                                 & instrument .~ Option
                                 & kind       .~ API.SellTrade
                                 & symbol     .~ "NFLX_071919P330"
                                 & price      ?~ 1.69 ])
          @?= res
-}
    ]
  where
    q12c300, qn12c300, q10c500, qn10c500, q12c500, qn12c500, q10c300, qn10c300
        :: Equity
    q12c300  =    12 @@ 300
    qn12c300 = (-12) @@ 300
    q10c500  =    10 @@ 500
    qn10c500 = (-10) @@ 500
    q12c500  =    12 @@ 500
    qn12c500 = (-12) @@ 500
    q10c300  =    10 @@ 300
    qn10c300 = (-10) @@ 300

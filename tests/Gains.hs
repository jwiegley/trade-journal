{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Gains where

import           Control.Lens
import           Control.Monad.Trans.Class
import           Hedgehog
-- import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range
import           Mock
import           Test.Tasty
-- import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog
import           ThinkOrSwim.Event

testGains :: TestTree
testGains = testGroup "gains"
    [ testProperty "gains_buy_sell_profit" gains_buy_sell_profit
    , testProperty "gains_buy_sell_profit_partial" gains_buy_sell_profit_partial
    ]

gains_buy_sell_profit :: Property
gains_buy_sell_profit = property $ do
    (_, st) <- withMockState "ZM" $ do
        q   <- lift $ forAll wide
        amt <- lift $ forAll big
        sub <- lift $ forAll little

        -- mockOptions.traceAll .= True
        let b = buy q (- amt + sub)
        submit b

        let s = sell q amt
        [CapitalGain _ g _, _] <- submit s

        g @?== b ^+ s

    st^?positionEvents.ix "ZM" @?== Nothing

gains_buy_sell_profit_partial :: Property
gains_buy_sell_profit_partial = property $ do
    (res, st) <- withMockState "ZM" $ do
        q   <- lift $ forAll wide
        amt <- lift $ forAll big
        sub <- lift $ forAll little

        let b = buy (q + 10) (- amt + sub)
        [a] <- submit b

        -- mockOptions.traceAll .= True
        let s = sell q amt
        [CapitalGain _ g _, _] <- submit s

        g @?== q * sub

        pure $ a^?!_OpenPosition._3
            & quantity .~ 10
            & cost     .~ 10 * (- amt + sub)

    st^?positionEvents.ix "ZM"
        @?== Just [ OpenPosition Long WashSaleIneligible res ]

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Trade.Provider.Coinmetro.Process where

import Amount
import Control.Lens hiding (Context)
import Data.Coerce (coerce)
import Data.Foldable
import Data.Text qualified as T
import Trade.Journal.Types qualified as Journal
import Trade.Provider.Coinmetro.Types

xactAction ::
  Transaction ->
  Amount 2 ->
  [Journal.Entry]
xactAction Transaction {..} _bal
  | "Order" `T.isInfixOf` _xactDescription =
      [ Journal.TradeEntry $
          Journal.Trade
            { tradeLot =
                Journal.Lot
                  { lotAmount = coerce _xactAmount,
                    lotDetail =
                      Journal.TimePrice
                        { price = coerce _xactPrice,
                          time = _xactDate
                        }
                  },
              tradeFees = coerce _xactFee
            }
      ]
  | "Deposit" `T.isInfixOf` _xactDescription
      || "Withdrawal" `T.isInfixOf` _xactDescription =
      [Journal.DepositEntry $ Journal.Deposit _xactAmount]
  | otherwise = []

coinmetroEntries ::
  [Transaction] ->
  [Journal.Entry]
coinmetroEntries cmXacts =
  snd $
    (\f -> foldr' f (0 :: Amount 2, []) cmXacts) $
      \xact (bal, rest) ->
        let bal' = bal + xact ^. xactAmount
         in (bal', xactAction xact bal' ++ rest)

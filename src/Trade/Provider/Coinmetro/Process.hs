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
xactAction xact@Transaction {..} _bal
  | "Order" `T.isInfixOf` _xactDescription =
      case _xactPrice of
        Left _ -> error $ "Order missing price: " ++ show xact
        Right p ->
          [ Journal.TradeEntry
              { tradeAssetFrom = _xactAsset,
                tradeAssetTo = _xactOtherCurrency,
                tradeCost =
                  either (const Nothing) Just _xactOtherAmount,
                tradeEntry =
                  Journal.Trade
                    { tradeLot =
                        Journal.Lot
                          { lotAmount = coerce _xactAmount,
                            lotDetail =
                              Journal.TimePrice
                                { price = coerce p,
                                  time = _xactDate
                                }
                          },
                      tradeFees = coerce _xactFee
                    }
              }
          ]
  | "Deposit" `T.isInfixOf` _xactDescription
      || "Withdrawal" `T.isInfixOf` _xactDescription =
      [ Journal.DepositEntry
          { depositAsset = _xactAsset,
            depositEntry = Journal.Deposit _xactAmount
          }
      ]
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

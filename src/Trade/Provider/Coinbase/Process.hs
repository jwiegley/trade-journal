{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Trade.Provider.Coinbase.Process where

import Amount
import Control.Lens hiding (Context)
import Data.Coerce (coerce)
import Data.Foldable
import Trade.Journal.Types qualified as Journal
import Trade.Provider.Coinbase.Types

xactAction ::
  Transaction ->
  Amount 2 ->
  [Journal.Entry]
xactAction Transaction {..} _bal
  | "Order" == _xactTransactionType =
      [ Journal.TradeEntry
          { tradeAssetFrom = undefined,
            tradeAssetTo = undefined,
            tradeCost = undefined,
            tradeEntry =
              Journal.Trade
                { tradeLot =
                    Journal.Lot
                      { lotAmount = coerce _xactQuantityTransacted,
                        lotDetail =
                          Journal.TimePrice
                            { price = coerce _xactPriceAtTransaction,
                              time = _xactTimestamp
                            }
                      },
                  tradeFees = coerce _xactFeesAndOrSpread
                }
          }
      ]
  | "Deposit" == _xactTransactionType
      || "Withdrawal" == _xactTransactionType =
      [ Journal.DepositEntry
          { depositAsset = undefined,
            depositEntry = Journal.Deposit _xactQuantityTransacted
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
        let bal' = bal + xact ^. xactQuantityTransacted
         in (bal', xactAction xact bal' ++ rest)

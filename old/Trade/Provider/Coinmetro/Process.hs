{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Trade.Provider.Coinmetro.Process where

import Amount
import Control.Lens hiding (Context)
import Data.Coerce (coerce)
import Data.Foldable
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Time.Format.ISO8601
import Trade.Journal.Entry qualified as Journal
import Trade.Journal.Types
import Trade.Provider.Coinmetro.Types

xactAction ::
  Context ->
  Transaction ->
  Amount 2 ->
  [Annotated Journal.Entry]
xactAction ctx@Context {..} Transaction {..} _bal
  | "Order" `T.isInfixOf` _xactDescription,
    _xactAsset == _currency =
      withAnn $
        Journal.TradeEntry $
          Journal.Trade
            { _tradeAction =
                if _xactAmount > 0
                  then Journal.Buy
                  else Journal.Sell,
              _tradeLot =
                Journal.Lot
                  { _amount = coerce _xactAmount,
                    _symbol = _xactAsset,
                    _price = coerce _xactPrice
                  },
              _tradeFees =
                Journal.Fees
                  { _fees = coerce _xactFee,
                    _commission = 0
                  }
            }
  | "Deposit" `T.isInfixOf` _xactDescription
      || "Withdrawal" `T.isInfixOf` _xactDescription =
      withAnn $ Journal.DepositEntry $ Journal.Deposit _xactAmount _account
  | otherwise = []
  where
    withAnn x =
      [ Annotated
          { _item = x,
            _time =
              fromMaybe
                ( error
                    "Could not parse time from Coinmetro"
                )
                (iso8601ParseM (T.unpack _xactDate)),
            _context = ctx,
            _details =
              [ Meta "Description" _xactDescription,
                Meta "IBAN" _xactIBAN,
                Meta "Transaction Hash" _xactTransactionHash,
                Meta "Address" _xactAddress,
                Meta "Tram" _xactTram,
                Meta "Additional Info" _xactAdditionalInfo,
                Meta "Reference Note" _xactReferenceNote,
                Meta "Comment" _xactComment
              ]
          }
      ]

coinmetroEntries ::
  Context ->
  [Transaction] ->
  [Annotated Journal.Entry]
coinmetroEntries ctx cmXacts =
  snd $
    (\f -> foldr' f (0 :: Amount 2, []) cmXacts) $
      \xact (bal, rest) ->
        let bal' = bal + xact ^. xactAmount
         in (bal', xactAction ctx xact bal' ++ rest)

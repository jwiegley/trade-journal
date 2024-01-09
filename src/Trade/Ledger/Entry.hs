{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Trade.Ledger.Entry where

import Amount
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Time
import Trade.Journal.Entry hiding (_amount, _price, _symbol)
import Trade.Journal.Types hiding (_account, _amount, _price, _symbol)
import Trade.Ledger hiding (account, amount, price)
import Prelude hiding (Double, Float)

depositTransaction :: Annotated Deposit -> Transaction (Annotated Deposit) 6
depositTransaction ann = case ann ^. item of
  Deposit amt acct ->
    Transaction
      { _actualDate = ann ^. time . to utctDay,
        _effectiveDate = Nothing,
        _code = "DEP",
        _payee =
          if amt < 0
            then "Withdraw from account"
            else "Deposit into account",
        _postings =
          [ Posting
              { _account = Other ("Assets:" <> ann ^. context . account),
                _isVirtual = False,
                _isBalancing = True,
                _amount =
                  CommodityAmount
                    CommodityLot
                      { _instrument = Miscellaneous,
                        _quantity = amt ^. coerced,
                        _symbol = "$",
                        _cost = Nothing,
                        _purchaseDate = Nothing,
                        _note = Nothing,
                        _price = Nothing
                      },
                _postMetadata = mempty
              },
            Posting
              { _account = Other acct,
                _isVirtual = False,
                _isBalancing = True,
                _amount = NullAmount,
                _postMetadata = mempty
              }
          ],
        _xactMetadata = mempty,
        _provenance = ann
      }
  Transfer lot acct ->
    Transaction
      { _actualDate = ann ^. time . to utctDay,
        _effectiveDate = Nothing,
        _code = "XFER",
        _payee =
          if lot ^. amount < 0
            then "Transfer from account"
            else "Transfer into account",
        _postings =
          [ Posting
              { _account = Other ("Assets:" <> ann ^. context . account),
                _isVirtual = False,
                _isBalancing = True,
                _amount =
                  CommodityAmount
                    CommodityLot
                      { _instrument = Miscellaneous,
                        _quantity = lot ^. amount . coerced,
                        _symbol = ann ^. context . currency,
                        _cost = Nothing,
                        _purchaseDate = Nothing,
                        _note = Nothing,
                        _price = Just (lot ^. price . coerced)
                      },
                _postMetadata = mempty
              },
            Posting
              { _account = Other acct,
                _isVirtual = False,
                _isBalancing = True,
                _amount = NullAmount,
                _postMetadata = mempty
              }
          ],
        _xactMetadata = mempty,
        _provenance = ann
      }

tradeTransaction :: Annotated Trade -> Transaction (Annotated Trade) 6
tradeTransaction ann = case ann ^. item of
  Trade {..} -> undefined

optionTradeTransaction ::
  Annotated OptionTrade ->
  Transaction (Annotated OptionTrade) 6
optionTradeTransaction ann = case ann ^. item of
  _ -> undefined

incomeTransaction :: Annotated Income -> Transaction (Annotated Income) 6
incomeTransaction ann = case ann ^. item of
  _ -> undefined

entryTransaction :: Annotated Entry -> Transaction (Annotated Entry) 6
entryTransaction entry =
  entry ^. item & \case
    TradeEntry trade ->
      tradeTransaction (trade <$ entry)
        & provenance %~ fmap TradeEntry
    OptionTradeEntry optionTrade ->
      optionTradeTransaction (optionTrade <$ entry)
        & provenance %~ fmap OptionTradeEntry
    IncomeEntry income ->
      incomeTransaction (income <$ entry)
        & provenance %~ fmap IncomeEntry
    DepositEntry deposit ->
      depositTransaction (deposit <$ entry)
        & provenance %~ fmap DepositEntry

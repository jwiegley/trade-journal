{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ledger.Entry where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time
-- import Text.Show.Pretty (ppShow)
-- import Debug.Trace (trace)
import Ledger hiding (account, price, symbol)
import Trade.Journal.Types

{-
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
-}

positionAssertionTransaction ::
  Text -> Text -> Text -> Position -> Transaction Position 2
positionAssertionTransaction cashAccount equitiesAccount symbol = \case
  pos@(Open (OpenPosition {..})) ->
    Transaction
      { _actualDate = utctDay (time (lotDetail openLot)),
        _effectiveDate = Nothing,
        _code = "TRADE",
        _payee =
          ( if lotAmount openLot < 0
              then "Sell"
              else "Buy"
          )
            <> " to Open "
            <> symbol,
        _postings =
          [ Posting
              { _account = Equities equitiesAccount,
                _isVirtual = False,
                _isBalancing = True,
                _amount =
                  CommodityAmount
                    CommodityLot
                      { _instrument = Miscellaneous,
                        _quantity = lotAmount openLot,
                        _symbol = symbol,
                        _cost = openBasis,
                        _purchaseDate =
                          Just (utctDay (time (lotDetail openLot))),
                        _note = Nothing,
                        _price = Just (price (lotDetail openLot))
                      },
                _postMetadata = mempty
              },
            Posting
              { _account = Cash cashAccount,
                _isVirtual = False,
                _isBalancing = True,
                _amount = NullAmount,
                _postMetadata = mempty
              }
          ],
        _xactMetadata = mempty,
        _provenance = pos
      }
  pos@(Closed (ClosedPosition {..})) ->
    Transaction
      { _actualDate = utctDay (time (lotDetail closingLot)),
        _effectiveDate = Nothing,
        _code = "TRADE",
        _payee =
          ( if lotAmount closingLot < 0
              then "Sell "
              else "Buy "
          )
            <> " to Open "
            <> symbol,
        _postings =
          [ Posting
              { _account = Cash cashAccount,
                _isVirtual = False,
                _isBalancing = True,
                _amount =
                  CommodityAmount
                    CommodityLot
                      { _instrument = Miscellaneous,
                        _quantity = lotAmount closingLot,
                        _symbol = symbol,
                        _cost = Nothing,
                        _purchaseDate =
                          Just (utctDay (time (lotDetail closingLot))),
                        _note = Nothing,
                        _price = Just (price (lotDetail closingLot))
                      },
                _postMetadata = mempty
              },
            Posting
              { _account = Equities equitiesAccount,
                _isVirtual = False,
                _isBalancing = True,
                _amount = NullAmount,
                _postMetadata = mempty
              }
          ],
        _xactMetadata = mempty,
        _provenance = pos
      }

transactionFromChanges ::
  Text ->
  Text ->
  Text ->
  Trade ->
  [PositionChange] ->
  Transaction (Trade, [PositionChange]) 2
transactionFromChanges
  cashAccount
  equitiesAccount
  symbol
  trade@(Trade lot fees commissions)
  changes =
    Transaction
      { _actualDate = utctDay (time (lotDetail lot)),
        _effectiveDate = Nothing,
        _code = "TRADE",
        _payee =
          ( if lotAmount lot < 0
              then "Sell "
              else "Buy "
          )
            <> symbol,
        _postings = concatMap go changes ++ feePostings,
        _xactMetadata = mempty,
        _provenance = (trade, changes)
      }
    where
      go :: PositionChange -> [Posting 2]
      go = \case
        PositionUnchanged _ -> []
        PositionOpen openLot ->
          openPostings openLot (lotAmount openLot)
        PositionIncrease (OpenPosition openLot _costBasis) n ->
          openPostings openLot n
        PositionPartialClose (OpenPosition openLot costBasis) closingLot ->
          -- trace ("openLot = " ++ ppShow openLot) $
          --   trace ("costBasis = " ++ ppShow costBasis) $
          --     trace ("closingLot = " ++ ppShow closingLot) $
          closePostings
            openLot
            costBasis
            (lotAmount closingLot)
            (lotDetail closingLot)
        PositionClose (OpenPosition openLot costBasis) tp ->
          closePostings openLot costBasis (lotAmount openLot) tp

      feePostings =
        [ Posting
            { _account = Fees,
              _isVirtual = False,
              _isBalancing = True,
              _amount = DollarAmount fees,
              _postMetadata = mempty
            }
          | fees > 0
        ]
          ++ [ Posting
                 { _account = Commissions,
                   _isVirtual = False,
                   _isBalancing = True,
                   _amount = DollarAmount commissions,
                   _postMetadata = mempty
                 }
               | commissions > 0
             ]

      openPostings openLot n =
        [ Posting
            { _account = Account equitiesAccount,
              _isVirtual = False,
              _isBalancing = True,
              _amount =
                CommodityAmount
                  CommodityLot
                    { _instrument = Miscellaneous,
                      _quantity = n,
                      _symbol = symbol,
                      _cost = Nothing,
                      _purchaseDate = Nothing,
                      _note = Nothing,
                      _price = Just (price (lotDetail openLot))
                    },
              _postMetadata = mempty
            },
          Posting
            { _account = Cash cashAccount,
              _isVirtual = False,
              _isBalancing = True,
              _amount =
                DollarAmount
                  (-price (lotDetail openLot) * n - fees - commissions),
              _postMetadata = mempty
            }
        ]

      closePostings openLot costBasis n tp =
        [ Posting
            { _account = Account equitiesAccount,
              _isVirtual = False,
              _isBalancing = True,
              _amount =
                CommodityAmount
                  CommodityLot
                    { _instrument = Miscellaneous,
                      _quantity = n,
                      _symbol = symbol,
                      _cost =
                        Just
                          ( fromMaybe
                              (price (lotDetail openLot))
                              costBasis
                          ),
                      _purchaseDate =
                        Just (utctDay (time (lotDetail openLot))),
                      _note = Nothing,
                      _price = Just (price tp)
                    },
              _postMetadata = mempty
            },
          Posting
            { _account = Cash cashAccount,
              _isVirtual = False,
              _isBalancing = True,
              _amount = DollarAmount (-price tp * n),
              _postMetadata = mempty
            },
          Posting
            { _account =
                if time tp `diffUTCTime` time (lotDetail openLot)
                  < 365 * 24 * 3600
                  then
                    if diff < 0
                      then CapitalGainShort
                      else CapitalLossShort
                  else
                    if diff < 0
                      then CapitalGainLong
                      else CapitalLossLong,
              _isVirtual = False,
              _isBalancing = True,
              _amount = DollarAmount (-diff * n),
              _postMetadata = mempty
            }
        ]
        where
          diff = price (lotDetail openLot) - price tp

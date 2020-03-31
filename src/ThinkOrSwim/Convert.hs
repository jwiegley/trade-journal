{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ThinkOrSwim.Convert where

import Control.Lens
import Data.Ledger as Ledger
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text as T
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API

convertTransaction :: API.Transaction
                   -> Ledger.Transaction API.Transaction 'Pending
convertTransaction xact@API.Transaction {..} =
    Ledger.Transaction
        { _date     = _transactionDate
        , _code     = T.pack (show _type_)
        , _payee    = _transactionDescription
        , _postings =
          [ Posting
              { _account   = Ledger.Fees
              , _isVirtual = True
              , _amount    = DollarAmount (xact^.fees_.regFee)
              } | xact^.fees_.regFee > 0 ]
            ++
          [ Posting
              { _account   = Ledger.Commissions
              , _isVirtual = True
              , _amount    = DollarAmount (xact^.fees_.commission)
              } | xact^.fees_.commission > 0 ]
            ++
          [ Posting
              { _account   =
                case xact^?transactionItem_.transactionInstrument._Just.assetType of
                    Just Equity                  -> Equities actId
                    Just MutualFund              -> Equities actId
                    Just (OptionAsset _)         -> Options  actId
                    Just (FixedIncomeAsset _)    -> Equities actId
                    Just (CashEquivalentAsset _) -> Equities actId
                    Nothing                      -> Cash actId
              , _isVirtual = False
              , _amount    =
                CommodityAmount
                    { _quantity     = fromMaybe 0
                        (xact^.transactionItem_.API.amount)
                    , _symbol       = fromMaybe "???"
                        (xact^?transactionItem_.transactionInstrument._Just.symbol)
                    , _cost         = Just (xact^.transactionItem_.cost)
                    , _purchaseDate =
                      -- jww (2020-03-29): NYI: Only if opening
                      Just (xact^.transactionDate)
                    , _refs         = Nothing -- jww (2020-03-29): NYI
                    , _price        = xact^.transactionItem_.price
                    }
              }
          , Posting
              { _account   = Ledger.Cash actId
              , _isVirtual = False
              , _amount    = DollarAmount (xact^.netAmount)
              }
          ]
        , _metadata = M.empty
        , _provenance = [xact]
        }
  where
    actId = T.pack (show (xact^.transactionItem_.accountId))

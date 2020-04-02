{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ThinkOrSwim.Convert (convertTransactions) where

import Control.Lens
import Data.Ledger as Ledger
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text as T
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API

convertTransactions :: OrderHistory
                    -> [Ledger.Transaction API.Transaction 'Pending]
convertTransactions (OrderHistory xacts) =
    Prelude.map convertTransaction xacts

convertTransaction :: API.Transaction
                   -> Ledger.Transaction API.Transaction 'Pending
convertTransaction t@API.Transaction {..} =
    Ledger.Transaction
        { _date     = t^.transactionInfo_.transactionDate
        , _code     = T.pack (show _type_)
        , _payee    = t^.transactionInfo_.transactionDescription
        , _postings =
          [ Posting
              { _account   = Ledger.Fees
              , _isVirtual = True
              , _amount    = DollarAmount (t^.fees_.regFee)
              } | t^.fees_.regFee > 0 ]
            ++
          [ Posting
              { _account   = Ledger.Commissions
              , _isVirtual = True
              , _amount    = DollarAmount (t^.fees_.commission)
              } | t^.fees_.commission > 0 ]
            ++
          [ Posting
              { _account   =
                case t^?transactionInfo_.transactionItem_.transactionInstrument._Just.assetType of
                    Just Equity                  -> Equities actId
                    Just MutualFund              -> Equities actId
                    Just (OptionAsset _)         -> Options  actId
                    Just (FixedIncomeAsset _)    -> Equities actId
                    Just (CashEquivalentAsset _) -> Equities actId
                    Nothing                      -> Cash     actId
              , _isVirtual = False
              , _amount    =
                CommodityAmount
                    { _quantity     = fromMaybe 0
                        (t^.transactionInfo_.transactionItem_.API.amount)
                    , _symbol       = fromMaybe "???"
                        (t^?transactionInfo_.transactionItem_.transactionInstrument._Just.symbol)
                    , _cost         = Just (t^.transactionInfo_.transactionItem_.cost)
                    , _purchaseDate =
                      -- jww (2020-03-29): NYI: Only if opening
                      Just (t^.transactionInfo_.transactionDate)
                    , _refs         = Nothing -- jww (2020-03-29): NYI
                    , _price        = t^.transactionInfo_.transactionItem_.price
                    }
              }
          , Posting
              { _account   = Ledger.Cash actId
              , _isVirtual = False
              , _amount    = DollarAmount (t^.netAmount)
              }
          ]
        , _metadata = M.empty
        , _provenance = [t]
        }
  where
    actId = T.pack (show (t^.transactionInfo_.transactionItem_.accountId))

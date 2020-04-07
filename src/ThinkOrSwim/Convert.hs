{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module ThinkOrSwim.Convert (convertTransactions) where

import Control.Lens
import Data.Ledger as Ledger
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text as T
import Data.Time
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API

convertTransactions :: TransactionHistory
                    -> [Ledger.Transaction API.Order]
convertTransactions hist =
    Prelude.map (convertTransaction (hist^.ordersMap)) (hist^.settlementList)

getOrder :: OrdersMap -> Either API.Transaction API.OrderId -> API.Order
getOrder _ (Left t) = orderFromTransaction t
getOrder m (Right oid) = m^?!ix oid

convertTransaction
    :: OrdersMap
    -> (UTCTime, Either API.Transaction API.OrderId)
    -> Ledger.Transaction API.Order
convertTransaction m (sd, getOrder m -> o) = Ledger.Transaction {..}
  where
    _actualDate    = sd
    _effectiveDate = Nothing
    _code          = T.pack (show (o^.orderType))
    _payee         = o^.orderDescription
    _metadata      = M.empty
    _provenance    = o
    _postings      =
        Prelude.concatMap
            (convertPostings (T.pack (show (o^.orderAccountId))))
            (o^.transactions)

convertPostings :: Text -> API.Transaction -> [Ledger.Posting]
convertPostings actId t =
    [ Posting
        { _account     = Ledger.Fees
        , _isVirtual   = True
        , _isBalancing = True
        , _amount      = DollarAmount (t^.fees_.regFee)
        } | t^.fees_.regFee > 0 ]
      ++
    [ Posting
        { _account     = Ledger.Commissions
        , _isVirtual   = True
        , _isBalancing = True
        , _amount      = DollarAmount (t^.fees_.commission)
        } | t^.fees_.commission > 0 ]
      ++
    [ Posting
        { _account     =
          case atype of
              Just Equity                  -> Equities actId
              Just MutualFund              -> Equities actId
              Just (OptionAsset _)         -> Options  actId
              Just (FixedIncomeAsset _)    -> Equities actId
              Just (CashEquivalentAsset _) -> Equities actId
              Nothing                      -> Cash     actId
        , _isVirtual   = False
        , _isBalancing = True
        , _amount      =
          CommodityAmount
              { _instrument   = case atype of
                  Just Equity                  -> Ledger.Stock
                  Just MutualFund              -> Ledger.Stock
                  Just (OptionAsset _)         -> Ledger.Option
                  Just (FixedIncomeAsset _)    -> Ledger.Stock
                  Just (CashEquivalentAsset _) -> Ledger.Stock
                  Nothing                      -> error "Unexpected"
              , _quantity     =
                  let n = fromMaybe 0 (t^.item.API.amount)
                  in (if t^.item.cost < 0 then n else (-n))
              , _symbol       = fromMaybe "???"
                  (t^?item.transactionInstrument._Just.symbol)
              , _cost         = Just (abs (t^.item.cost))
              , _purchaseDate = Just (t^.transactionInfo_.transactionDate)
              , _refs         = Nothing
              , _price        = t^.item.price
              }
        } ]
      ++
    [ Posting
        { _account     = Ledger.Cash actId
        , _isVirtual   = False
        , _isBalancing = True
        , _amount      = DollarAmount (t^.netAmount)
        }
    ]
  where
    item :: Lens' API.Transaction TransactionItem
    item = transactionInfo_.transactionItem_

    atype = t^?item.transactionInstrument._Just.assetType

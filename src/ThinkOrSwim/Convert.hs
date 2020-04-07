{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module ThinkOrSwim.Convert (convertTransactions) where

import Control.Lens
import Data.Ledger as Ledger
import Data.Map as M
import Data.Maybe (isJust, isNothing, fromMaybe)
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
    _code          = o^.orderId
    _payee         = o^.orderDescription
    _xactMetadata  = M.empty
        & at "Type" ?~ T.pack (show (o^.orderType))
    _provenance    = o
    _postings      =
        Prelude.concatMap
            (convertPostings (T.pack (show (o^.orderAccountId))))
            (o^.transactions)

convertPostings :: Text -> API.Transaction -> [Ledger.Posting]
convertPostings _ t
    | t^.transactionInfo_.transactionSubType == TradeCorrection = []
convertPostings actId t =
    [ Posting
        { _account      = Ledger.Fees
        , _isVirtual    = True
        , _isBalancing  = True
        , _amount       = DollarAmount (t^.fees_.regFee)
        , _postMetadata = M.empty
        }
    | t^.fees_.regFee /= 0 ]
      ++
    [ Posting
        { _account      = Ledger.Charges
        , _isVirtual    = True
        , _isBalancing  = True
        , _amount       = DollarAmount (t^.fees_.otherCharges)
        , _postMetadata = M.empty
        }
    | t^.fees_.otherCharges /= 0 ]
      ++
    [ Posting
        { _account      = Ledger.Commissions
        , _isVirtual    = True
        , _isBalancing  = True
        , _amount       = DollarAmount (t^.fees_.commission)
        , _postMetadata = M.empty
        }
    | t^.fees_.commission /= 0 ]
      ++
    [ post act CommodityAmount
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
          , _symbol       = sym
          , _cost         = if cst /= 0 then Just cst else Nothing
          , _purchaseDate = Just date
          , _refs         = [Ref OpeningOrder xactId]
          , _price        = t^.item.price
          }
          & postMetadata .~
              (M.empty & at "SubType" ?~ T.pack (show subtyp)
                       & at "XID"     ?~ T.pack (show xactId))
    | isJust (t^.item.API.amount) ]
      ++
    [ case t^.item.price of
          Just _  -> post (Ledger.Cash actId) (DollarAmount (t^.netAmount))
          Nothing
              | t^.netAmount /= 0 -> post (Cash actId) (DollarAmount (t^.netAmount))
              | otherwise -> post OpeningBalances NoAmount
    | case t^.item.price of
          Just _  -> t^.netAmount /= 0
          Nothing -> True
    ]
      ++
    [ post OpeningBalances NoAmount
    | isNothing (t^.item.API.amount)
    ]
  where
    item :: Lens' API.Transaction TransactionItem
    item = transactionInfo_.transactionItem_

    xactId = t^.transactionInfo_.transactionId
    subtyp = t^.transactionInfo_.transactionSubType
    date   = t^.transactionInfo_.transactionDate

    sym =
        let s = fromMaybe "???" (t^?item.transactionInstrument._Just.symbol)
        in if s == " "
           then fromMaybe "!!!" (t^?item.transactionInstrument._Just.cusip)
           else s

    act  = case atype of
        Just Equity                  -> Equities actId
        Just MutualFund              -> Equities actId
        Just (OptionAsset _)         -> Options  actId
        Just (FixedIncomeAsset _)    -> Equities actId
        Just (CashEquivalentAsset _) -> Equities actId
        Nothing                      -> Cash     actId

    fees' = t^.fees_.regFee + t^.fees_.otherCharges + t^.fees_.commission
    cst   = abs (t^.item.cost - fees')
    atype = t^?item.transactionInstrument._Just.assetType

    post a m = Posting
        { _account      = a
        , _isVirtual    = False
        , _isBalancing  = True
        , _amount       = m
        , _postMetadata = M.empty
        }

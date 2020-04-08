{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module ThinkOrSwim.Convert (convertTransactions) where

import           Control.Lens
import           Control.Monad.State
import           Data.Ledger as Ledger
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (isJust, isNothing, fromMaybe)
import           Data.Text as T
import           Data.Text.Lens
import           Data.Time
import           ThinkOrSwim.API.TransactionHistory.GetTransactions as API

convertTransactions :: TransactionHistory
                    -> [Ledger.Transaction API.Order]
convertTransactions hist = (`evalState` M.empty) $
    Prelude.mapM (convertTransaction (hist^.ordersMap)) (hist^.settlementList)

getOrder :: OrdersMap -> Either API.Transaction API.OrderId -> API.Order
getOrder _ (Left t) = orderFromTransaction t
getOrder m (Right oid) = m^?!ix oid

type OpenTransactions = Map Text [TransactionItem]

convertTransaction
    :: OrdersMap
    -> (UTCTime, Either API.Transaction API.OrderId)
    -> State OpenTransactions (Ledger.Transaction API.Order)
convertTransaction m (sd, getOrder m -> o) = do
    let _actualDate    = sd
        _effectiveDate = Nothing
        _code          = o^.orderId
        _payee         = o^.orderDescription
        _xactMetadata  =
            M.empty & at "Type"   ?~ T.pack (show (o^.orderType))
                    & at "Symbol" ?~ underlying
        _provenance    = o
    _postings <-
        Prelude.concat <$> mapM
            (convertPostings (T.pack (show (o^.orderAccountId))))
            (o^.transactions)
    pure Ledger.Transaction {..}
  where
    underlying =
        let xs = Prelude.map (^.baseSymbol) (o^.transactions)
        in if Prelude.all (== Prelude.head xs) (Prelude.tail xs)
           then Prelude.head xs
           else error $ "Transaction deals with various symbols: " ++ show xs

convertPostings :: Text -> API.Transaction
                -> State OpenTransactions [Ledger.Posting]
convertPostings _ t
    | t^.transactionInfo_.transactionSubType == TradeCorrection = pure []
convertPostings actId t = posts <$> gainsKeeper t lot
  where
    posts c =
        [ post Ledger.Fees True (DollarAmount (t^.fees_.regFee))
        | t^.fees_.regFee /= 0 ]
          ++
        [ post Ledger.Charges True (DollarAmount (t^.fees_.otherCharges))
        | t^.fees_.otherCharges /= 0 ]
          ++
        [ post Ledger.Commissions True (DollarAmount (t^.fees_.commission))
        | t^.fees_.commission /= 0 ]
          ++
        [ post act False (CommodityAmount c) & postMetadata .~ meta
        | isJust (t^.item.API.amount) ]
          ++
        [ case t^.item.API.price of
              Just _                     -> cashPost
              Nothing | t^.netAmount /= 0 -> cashPost
                      | otherwise        -> post act False NoAmount
        | case t^.item.API.price of
              Just _  -> t^.netAmount /= 0
              Nothing -> not fromEquity ]
          ++
        [ post OpeningBalances False NoAmount
        | isNothing (t^.item.API.amount) || fromEquity ]

    lot = CommodityLot
        { _instrument = case atype of
            Just Equity               -> Ledger.Stock
            Just MutualFund           -> Ledger.Stock
            Just (OptionAsset _)      -> Ledger.Option
            Just (FixedIncomeAsset _) -> Ledger.Bond
            Just (CashEquivalentAsset
                  CashMoneyMarket)    -> Ledger.MoneyMarket
            Nothing                   -> error "Unexpected"

        , _quantity =
            let n = fromMaybe 0 (t^.item.API.amount)
            in case t^.item.instruction of Just Sell -> (-n); _ -> n
        , _symbol   = sym
        , _price    = t^.item.API.price

        , _cost         = if cst /= 0 then Just cst else Nothing
        , _purchaseDate = Just date
        , _refs         = [Ref OpeningOrder xactId]
        }

    cashPost = post (Cash actId) False (DollarAmount (t^.netAmount))

    xactId = t^.transactionInfo_.transactionId
    date   = t^.transactionInfo_.transactionDate

    meta = M.empty
        & at "Subtype"     ?~ T.pack (show subtyp)
        & at "XID"         ?~ T.pack (show xactId)
        & at "Instruction" .~ t^?item.instruction._Just.to show.packed
        & at "Effect"      .~ t^?item.positionEffect._Just.to show.packed
        & at "CUSIP"       .~ t^?instr._Just.cusip
        & at "Instrument"  .~ t^?instr._Just.assetType.to assetKind
        & at "Side"        .~ t^?option'.putCall.to show.packed
        & at "Strike"      .~ t^?option'.strikePrice._Just.to (thousands . Right)
        & at "Expiration"  .~ t^?option'.expirationDate.to iso8601
        & at "Contract"    .~ t^?option'.description

    instr :: Lens' API.Transaction (Maybe API.Instrument)
    instr = item.transactionInstrument

    option' :: Traversal' API.Transaction Option
    option' = instr._Just.assetType._OptionAsset

    sym = case t^.instr of
        Nothing -> error $ "Transaction instrument missing for XID " ++ show xactId
        Just inst -> case inst^.API.symbol of
            " " -> inst^.cusip
            s   -> s

    fromEquity = subtyp `elem` [ TransferOfSecurityOrOptionIn
                          , OptionAssignment
                          , OptionExpiration ]

    act = case atype of
        Just Equity                  -> Equities actId
        Just MutualFund              -> Equities actId
        Just (OptionAsset _)         -> Options  actId
        Just (FixedIncomeAsset _)    -> Bonds actId
        Just (CashEquivalentAsset _) -> MoneyMarkets actId
        Nothing                      -> OpeningBalances

    fees'  = t^.fees_.regFee + t^.fees_.otherCharges + t^.fees_.commission
    cst    = abs (t^.item.API.cost - fees')
    atype  = t^?instr._Just.assetType
    subtyp = t^.transactionInfo_.transactionSubType

    post a b m = Posting
        { _account      = a
        , _isVirtual    = b
        , _isBalancing  = not b
        , _amount       = m
        , _postMetadata = M.empty
        }

-- The function replicates the logic used by GainsKeeper to determine what
-- impact a given transaction, based on existing positions, should have on an
-- account.
gainsKeeper :: API.Transaction -> CommodityLot
            -> State OpenTransactions CommodityLot
gainsKeeper _t lot = pure lot

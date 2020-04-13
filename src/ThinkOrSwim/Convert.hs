{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module ThinkOrSwim.Convert (convertTransactions) where

import           Control.Lens
import           Control.Monad.State
import           Data.Amount
import           Data.Coerce
import           Data.Ledger as Ledger
import qualified Data.Map as M
import           Data.Maybe (isNothing)
import           Data.Text as T
import           Data.Text.Lens
import           Data.Time
import           Prelude hiding (Float, Double)
import           ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import           ThinkOrSwim.Gains
import           ThinkOrSwim.Types

-- import           Debug.Trace

convertTransactions
    :: OpenTransactions
    -> TransactionHistory
    -> [Ledger.Transaction API.Order]
convertTransactions m hist = (`evalState` m) $
    Prelude.mapM (convertTransaction (hist^.ordersMap)) (hist^.settlementList)

getOrder :: OrdersMap -> Either API.Transaction API.OrderId -> API.Order
getOrder _ (Left t) = orderFromTransaction t
getOrder m (Right oid) = m^?!ix oid

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
                    & at "Symbol" .~ case underlying of "" -> Nothing; s -> Just s
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
convertPostings actId t =
    posts <$> case t^.item.API.amount of
        Just _  -> gainsKeeper t lot
        Nothing -> pure []
  where
    posts cs =
        [ post Ledger.Fees True (DollarAmount (t^.fees_.regFee))
        | t^.fees_.regFee /= 0 ]
          ++
        [ post Ledger.Charges True (DollarAmount (t^.fees_.otherCharges))
        | t^.fees_.otherCharges /= 0 ]
          ++
        [ post Ledger.Commissions True (DollarAmount (t^.fees_.commission))
        | t^.fees_.commission /= 0 ]
          ++
        (flip Prelude.concatMap cs $ \(gain, cmdtyLot) ->
            if | gain < 0  ->
                 [ post CapitalLossShort False (DollarAmount gain) ]
               | gain > 0  ->
                 [ post CapitalGainShort False (DollarAmount gain) ]
               | otherwise ->
                 []
            ++ [ post act False (CommodityAmount cmdtyLot) & postMetadata .~ meta ])
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
        | needsBalancingPost ]

    needsBalancingPost = isNothing (t^.item.API.amount) || fromEquity

    lot = newCommodityLot
        & Ledger.instrument .~ case atype of
            Just Equity               -> Ledger.Stock
            Just MutualFund           -> Ledger.Stock
            Just (OptionAsset _)      -> Ledger.Option
            Just (FixedIncomeAsset _) -> Ledger.Bond
            Just (CashEquivalentAsset
                  CashMoneyMarket)    -> Ledger.MoneyMarket
            Nothing                   -> error "Unexpected"

        & Ledger.symbol   .~ symbolName t
        & Ledger.price    .~ coerce (t^.item.API.price)
        & Ledger.quantity .~ coerce (getXactAmount t)

    cashPost = post (Cash actId) False (DollarAmount (t^.netAmount))

    meta = M.empty
        & at "Subtype"     ?~ T.pack (show subtyp)
        & at "XID"         ?~ T.pack (show (t^.xactId))
        & at "Instruction" .~ t^?item.instruction._Just.to show.packed
        & at "Effect"      .~ t^?item.positionEffect._Just.to show.packed
        & at "CUSIP"       .~ t^?instr._Just.cusip
        & at "Instrument"  .~ t^?instr._Just.assetType.to assetKind
        & at "Side"        .~ t^?option'.putCall.to show.packed
        & at "Strike"      .~ t^?option'.strikePrice._Just.to thousands.packed
        & at "Expiration"  .~ t^?option'.expirationDate.to iso8601
        & at "Contract"    .~ t^?option'.description

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

    atype  = t^?instr._Just.assetType
    subtyp = t^.transactionInfo_.transactionSubType

    post a b m = Posting
        { _account      = a
        , _isVirtual    = b
        , _isBalancing  = not b
        , _amount       = m
        , _postMetadata = M.empty
        }

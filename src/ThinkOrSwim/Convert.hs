{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module ThinkOrSwim.Convert (convertOrders) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Data.Amount
import           Data.Coerce
import           Data.Ledger as L
import qualified Data.Map as M
import           Data.Maybe (isNothing)
import           Data.Text as T
import           Data.Text.Lens
import           Data.Time
import           Data.Time.Format.ISO8601
import           Prelude hiding (Float, Double, (<>))
import           ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import           ThinkOrSwim.Fixup
import           ThinkOrSwim.Gains
import           ThinkOrSwim.Options (Options)
import           ThinkOrSwim.Types

convertOrders
    :: Options
    -> GainsKeeperState API.TransactionSubType
    -> TransactionHistory
    -> [L.Transaction API.TransactionSubType API.Order L.LotAndPL]
convertOrders opts st hist = (`evalState` st) $
    Prelude.mapM (convertOrder opts (hist^.ordersMap))
                 (hist^.settlementList)

getOrder :: OrdersMap -> Either API.Transaction API.OrderId -> API.Order
getOrder _ (Left t)    = orderFromTransaction t
getOrder m (Right oid) = m^?!ix oid

convertOrder
    :: Options
    -> OrdersMap
    -> (Day, Either API.Transaction API.OrderId)
    -> State (GainsKeeperState API.TransactionSubType)
            (L.Transaction API.TransactionSubType API.Order L.LotAndPL)
convertOrder opts m (sd, getOrder m -> o) = do
    let _actualDate    = sd
        _effectiveDate = Nothing
        _code          = o^.orderId
        _payee         = o^.orderDescription
        _xactMetadata  =
            M.empty & at "Type"   ?~ T.pack (show (o^.orderType))
                    & at "Symbol" .~ case base of "" -> Nothing; s -> Just s
        _provenance    = o
    _postings <- Prelude.concat <$>
        mapM (convertPostings opts (T.pack (show (o^.orderAccountId))))
             (o^.transactions)
    fixupTransaction L.Transaction {..}
  where
    base | Prelude.all (== Prelude.head xs) (Prelude.tail xs) = Prelude.head xs
         | otherwise =
               error $ "Transaction deals with various symbols: " ++ show xs
        where
            xs = Prelude.map (^.baseSymbol) (o^.transactions)

transactionFees :: API.Transaction-> Amount 2
transactionFees t
    = t^.fees_.regFee
    + t^.fees_.otherCharges
    + t^.fees_.commission

convertTransaction :: API.Transaction -> Amount 6
                   -> CommodityLot API.TransactionSubType
convertTransaction t n = newCommodityLot @API.TransactionSubType
    & instrument   .~ instr
    & kind         .~ t^.xactSubType
    & L.quantity   .~ quant
    & L.symbol     .~ symbolName t
    & L.underlying .~ t^.baseSymbol
    & L.cost       ?~ coerce (abs (t^.item.API.cost))
    & purchaseDate ?~ utctDay (t^.xactDate)
    & washEligible .~ True
    & lotId        .~ t^.xactId
    & refs         .~ [ transactionRef t ]
    & L.price      .~ fmap coerce (t^.item.API.price)
  where
    quant = coerce (case t^.item.instruction of Just Sell -> -n; _ -> n)

    instr = case t^?instrument_._Just.assetType of
        Just API.Equity           -> L.Equity
        Just MutualFund           -> L.Equity
        Just (OptionAsset _)      -> L.Option
        Just (FixedIncomeAsset _) -> L.Bond
        Just (CashEquivalentAsset
              CashMoneyMarket)    -> L.MoneyMarket
        Nothing                   -> error "Unexpected"

convertPostings
    :: Options
    -> Text
    -> API.Transaction
    -> State (GainsKeeperState API.TransactionSubType)
            [L.Posting API.TransactionSubType L.LotAndPL]
convertPostings _ _ t
    | t^.transactionInfo_.transactionSubType == TradeCorrection = pure []
convertPostings opts actId t = posts <$>
    case t^.item.API.amount of
        Nothing -> pure []
        Just n  -> gainsKeeper opts (transactionFees t) maybeNet
                              (convertTransaction t n)
  where
    posts cs
        = [ post L.Fees True (DollarAmount (t^.fees_.regFee))
          | t^.fees_.regFee /= 0 ]
       ++ [ post L.Charges True (DollarAmount (t^.fees_.otherCharges))
          | t^.fees_.otherCharges /= 0 ]
       ++ [ post L.Commissions True (DollarAmount (t^.fees_.commission))
          | t^.fees_.commission /= 0 ]

       ++ (flip Prelude.concatMap cs $ \pl ->
            [ post act False (CommodityAmount pl)
                  & postMetadata %~ meta pl
            | pl^.plKind /= Rounding ])

       ++ [ case t^.item.API.price of
                Just _              -> cashPost
                Nothing | isPriced  -> cashPost
                        | otherwise -> post act False NoAmount
          | case t^.item.API.price of
                Just _  -> isPriced
                Nothing -> not fromEquity ]
       ++ [ post OpeningBalances False NoAmount
          | isNothing (t^.item.API.amount) || fromEquity ]
      where
        post = newPosting

    meta pl m = m
        & at "XType"        ?~ T.pack (show subtyp)
        & at "XId"          ?~ T.pack (show (t^.xactId))
        & at "XDate"        ?~ T.pack (iso8601Show (t^.xactDate))
        & at "Instruction"  .~ t^?item.instruction._Just.to show.packed
        & at "Effect"       .~ (t^?item.positionEffect._Just.to show.packed
                                  <|> Just (if pl^.plLoss == 0
                                            then "Opening"
                                            else "Closing"))
        & at "CUSIP"        .~ t^?instrument_._Just.cusip
        & at "Instrument"   .~ t^?instrument_._Just.assetType.to assetKind
        & at "Side"         .~ t^?option'.putCall.to show.packed
        & at "Strike"       .~ t^?option'.strikePrice._Just.to thousands.packed
        & at "Expiration"   .~ t^?option'.expirationDate.to (T.pack . iso8601Show)
        & at "Contract"     .~ t^?option'.description
        & at "WashDeferred" .~ pl^?plLot.washDeferred._Just.to show.packed

    act = case atype of
        Just API.Equity              -> Equities actId
        Just MutualFund              -> Equities actId
        Just (OptionAsset _)         -> Options  actId
        Just (FixedIncomeAsset _)    -> Bonds actId
        Just (CashEquivalentAsset _) -> MoneyMarkets actId
        Nothing                      -> OpeningBalances

    atype      = t^?instrument_._Just.assetType
    subtyp     = t^.transactionInfo_.transactionSubType
    isPriced   = t^.netAmount /= 0 || subtyp `elem` [ OptionExpiration ]
    direct     = cashXact || isNothing (t^.item.API.amount) || fromEquity
    maybeNet   = if direct then Nothing else Just (t^.netAmount)
    fromEquity = subtyp `elem` [ TransferOfSecurityOrOptionIn ]
    cashXact   = subtyp `elem` [ CashAlternativesPurchase
                          , CashAlternativesRedemption ]

    cashPost = newPosting (Cash actId) False $
        if t^.netAmount == 0
        then NoAmount
        else DollarAmount (t^.netAmount)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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
import           Data.Foldable
import qualified Data.Ledger as L
import           Data.Ledger hiding (symbol, quantity, cost, price)
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe, isNothing, mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lens
import           Data.Time
import           Data.Time.Format.ISO8601
import           Prelude hiding (Float, Double, (<>))
import qualified ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import           ThinkOrSwim.API.TransactionHistory.GetTransactions hiding (symbol, cost)
import           ThinkOrSwim.Event
import           ThinkOrSwim.Options (Options)
import           ThinkOrSwim.Types

convertOrders
    :: Options
    -> TransactionHistory
    -> State (GainsKeeperState API.Transaction)
            [L.Transaction API.TransactionSubType API.Order L.CommodityLot]
convertOrders opts hist =
    catMaybes <$>
        Prelude.mapM (convertOrder opts (hist^.ordersMap))
                     (hist^.settlementList)

getOrder :: OrdersMap -> Either API.Transaction API.OrderId -> API.Order
getOrder _ (Left t)    = orderFromTransaction t
getOrder m (Right oid) = m^?!ix oid

convertOrder
    :: Options
    -> OrdersMap
    -> (Day, Either API.Transaction API.OrderId)
    -> State (GainsKeeperState API.Transaction)
            (Maybe (L.Transaction API.TransactionSubType
                                  API.Order L.CommodityLot))
convertOrder opts m (sd, getOrder m -> o) = do
    let _actualDate    = sd
        _effectiveDate = Nothing
        _code          = o^.orderId
        _payee         = o^.orderDescription
        _xactMetadata  =
            M.empty & at "Type"   ?~ T.pack (show (o^.orderType))
                    & at "Symbol" .~ base
        _provenance    = o
    _postings <- Prelude.concat <$>
        mapM (convertPostings opts (T.pack (show (o^.orderAccountId))))
             (o^.transactions)
    pure $ case _postings of
        [] -> Nothing
        _  -> Just L.Transaction {..}
  where
    base | Prelude.all (== Prelude.head xs) (Prelude.tail xs) = Prelude.head xs
         | otherwise =
               error $ "Transaction deals with various symbols: " ++ show xs
        where
            xs = Prelude.map (^.xunderlying) (o^.transactions)

convertPostings
    :: Options
    -> Text
    -> API.Transaction
    -> State (GainsKeeperState API.Transaction)
            [L.Posting API.TransactionSubType L.CommodityLot]
convertPostings _ _ t | t^.xsubType == TradeCorrection = pure []
convertPostings opts actId t = do
    events <- gainsKeeper opts t
    pure $ case events of
        [] -> []
        xs -> roundPostings (posts xs)
  where
    posts cs
        = [ newPosting L.Fees True (DollarAmount (t^.fees_.regFee))
          | t^.fees_.regFee /= 0 ]
       ++ [ newPosting L.Charges True (DollarAmount (t^.fees_.otherCharges))
          | t^.fees_.otherCharges /= 0 ]
       ++ [ newPosting L.Commissions True (DollarAmount (t^.fees_.commission))
          | t^.fees_.commission /= 0 ]

       ++ (flip mapMaybe cs $ \ev ->
             postingFromEvent actId ev & each.postMetadata %~ meta ev)

       ++ [ case t^?xprice of
                Just _              -> cashPost
                Nothing | isPriced  -> cashPost
                        | otherwise -> newPosting act False NoAmount
          | case t^?xprice of
                Just _  -> isPriced
                Nothing -> not fromEquity ]
       ++ [ newPosting OpeningBalances False NoAmount
          | isNothing (t^?xamount) || fromEquity ]

    meta ev m = m
        & at "XId"          ?~ T.pack (show (t^.xid))
        & at "XType"        ?~ T.pack (show subtyp)
        -- & at "XDate"        ?~ T.pack (iso8601Show (t^.xdate))
        -- & at "Instruction"  .~ t^?xinstruction._Just.to show.packed
        -- & at "CUSIP"        .~ t^.xcusip
        -- & at "Instrument"   .~ t^?xasset.to assetKind
        -- & at "Side"         .~ t^?xoption.putCall.to show.packed
        -- & at "Strike"       .~ t^?xoption.strikePrice._Just.to thousands.packed
        -- & at "Expiration"   .~ t^?xoption.expirationDate.to (T.pack . iso8601Show)
        -- & at "Contract"     .~ t^?xoption.description
        & at "Effect"       .~ (effectDesc ev <|>
                                t^?xitem.positionEffect.each.to show.packed)
        -- & at "WashDeferred" .~ pl^?plLot.washDeferred._Just.to show.packed

    effectDesc = \case
        OpenPosition disp _     -> Just $ T.pack $ "Open " ++ show disp
        PositionClosed disp _ _ -> Just $ T.pack $ "Close " ++ show disp
        _ -> Nothing

    act          = transactionAccount actId t
    subtyp       = t^.transactionInfo_.transactionSubType
    isPriced     = t^.netAmount /= 0 || subtyp `elem` [ OptionExpiration ]
    fromEquity   = subtyp `elem` [ TransferIn ]
    -- cashXact     = subtyp `elem` [ CashAlternativesPurchase
    --                         , CashAlternativesRedemption ]
    -- direct       = cashXact || not (has xamount t) || fromEquity
    -- maybeNet     = if direct then Nothing else Just (t^.netAmount)

    cashPost = newPosting (Cash actId) False $
        if t^.netAmount == 0
        then NoAmount
        else DollarAmount (t^.netAmount)

    roundPostings ps =
        let slip = - sumPostings ps
        in ps ++ [ newPosting L.Commissions False (DollarAmount slip)
                 | slip /= 0 && abs slip < 0.02 ]

-- The idea of this function is to replicate what Ledger will calculate the
-- sum to be, so that if there's any discrepancy we can add a rounding
-- adjustment to pass the balancing check.
sumPostings :: [L.Posting API.TransactionSubType L.CommodityLot] -> Amount 2
sumPostings = foldl' go 0
  where
    norm = normalizeAmount mpfr_RNDNA

    cst l | l^.isVirtual = 0
          | Just n <- l^?L.amount._DollarAmount = norm n
          | Just q <- l^?L.amount._CommodityAmount.L.quantity,
            Just n <- l^?L.amount._CommodityAmount.L.cost.each =
              let n' | q < 0     = -n
                     | otherwise = n
              in norm (coerce n')
          | otherwise = 0

    go acc pl = acc + cst pl

transactionAccount :: Text -> API.Transaction -> L.Account
transactionAccount actId t = case t^?xasset of
    Just API.Equity              -> Equities actId
    Just MutualFund              -> Equities actId
    Just (OptionAsset _)         -> Options  actId
    Just (FixedIncomeAsset _)    -> Bonds actId
    Just (CashEquivalentAsset _) -> MoneyMarkets actId
    Nothing                      -> OpeningBalances

mkCommodityLot :: Lot API.Transaction
               -> CommodityLot API.TransactionSubType
mkCommodityLot t = newCommodityLot @API.TransactionSubType
    & instrument   .~ instr
    & L.quantity   .~ t^.quantity.coerced
    & L.symbol     .~ fromMaybe "???" (t^.symbol)
    & L.cost       ?~ t^.cost.coerced.to abs
    & purchaseDate ?~ utctDay (t^.time)
    & refs         .~ [ transactionRef (t^.xact) ]
    & L.price      .~ t^?xact.xprice.coerced
  where
    instr = case t^?xact.xasset of
        Just API.Equity           -> L.Equity
        Just MutualFund           -> L.Equity
        Just (OptionAsset _)      -> L.Option
        Just (FixedIncomeAsset _) -> L.Bond
        Just (CashEquivalentAsset
              CashMoneyMarket)    -> L.MoneyMarket
        Nothing                   -> error "Unexpected"

postingFromEvent
    :: Text
    -> Event (Lot API.Transaction)
    -> Maybe (L.Posting API.TransactionSubType L.CommodityLot)
postingFromEvent actId ev = case ev of
    OpenPosition disp o -> Just $
        newPosting (transactionAccount actId (o^.xact)) False
          (CommodityAmount $ mkCommodityLot o
             & L.quantity %~ case disp of Long -> id; Short -> negate)

    PositionClosed disp o c -> Just $
        newPosting (transactionAccount actId (o^.xact)) False
          (CommodityAmount $ mkCommodityLot o
             & L.quantity %~ case disp of Long -> negate; Short -> id
             & L.price    .~ c^?xact.xprice.coerced)

    OptionAssigned o c -> Just $
        newPosting (transactionAccount actId (o^.xact)) False
          (CommodityAmount $ mkCommodityLot o
             & L.quantity %~ (if isCall o then id else negate)
             & L.price    .~ if isOption c
                             then Just 0
                             else c^?xact.xprice.coerced)

    WashLossApplied _ _ g -> Just $
        newPosting CapitalWashLoss False (DollarAmount (g^.coerced))

    CapitalGain disp g _ -> Just $
        newPosting
          (case disp of Long  -> CapitalGainLong
                        Short -> CapitalGainShort)
          False (DollarAmount (g^.coerced.to negate))

    CapitalLoss disp g _ -> Just $
        newPosting
          (case disp of Long  -> CapitalLossLong
                        Short -> CapitalLossShort)
          False (DollarAmount (g^.coerced.to negate))

    UnrecognizedTransaction t -> Just $
        newPosting Unknown False
          (case t^.cost.coerced of
               n | n == 0    -> NoAmount
                 | otherwise -> DollarAmount n)

    _ -> Nothing

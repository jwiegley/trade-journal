{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module ThinkOrSwim.Convert (convertTransactions) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Data.Amount
import           Data.Coerce
import           Data.Ledger as Ledger
import qualified Data.Map as M
import           Data.Maybe (isNothing, fromMaybe)
import           Data.Text as T
import           Data.Text.Lens
import           Data.Time
import           Data.Time.Format.ISO8601
import           Prelude hiding (Float, Double)
import           ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import           ThinkOrSwim.Gains
import           ThinkOrSwim.Types

convertTransactions
    :: GainsKeeperState API.Transaction
    -> TransactionHistory
    -> [Ledger.Transaction API.Order API.Transaction]
convertTransactions st hist = (`evalState` st) $
    Prelude.mapM (convertTransaction (hist^.ordersMap)) (hist^.settlementList)

getOrder :: OrdersMap -> Either API.Transaction API.OrderId -> API.Order
getOrder _ (Left t)    = orderFromTransaction t
getOrder m (Right oid) = m^?!ix oid

convertTransaction
    :: OrdersMap
    -> (Day, Either API.Transaction API.OrderId)
    -> State (GainsKeeperState API.Transaction)
            (Ledger.Transaction API.Order API.Transaction)
convertTransaction m (sd, getOrder m -> o) = do
    let _actualDate    = sd
        _effectiveDate = Nothing
        _code          = o^.orderId
        _payee         = o^.orderDescription
        _xactMetadata  =
            M.empty & at "Type"   ?~ T.pack (show (o^.orderType))
                    & at "Symbol" .~ case underlying of "" -> Nothing; s -> Just s
        _provenance    = o
    _postings <- Prelude.concat <$>
        mapM (convertPostings (T.pack (show (o^.orderAccountId))))
             (o^.transactions)
    pure Ledger.Transaction {..}
  where
    underlying
        | Prelude.all (== Prelude.head xs) (Prelude.tail xs) = Prelude.head xs
        | otherwise =
              error $ "Transaction deals with various symbols: " ++ show xs
        where
            xs = Prelude.map (^.baseSymbol) (o^.transactions)

convertPostings
    :: Text
    -> API.Transaction
    -> State (GainsKeeperState API.Transaction) [Ledger.Posting API.Transaction]
convertPostings _ t
    | t^.transactionInfo_.transactionSubType == TradeCorrection = pure []
convertPostings actId t = posts <$> case t^.item.API.amount of
    Just n -> gainsKeeper t $ newCommodityLot
        & Ledger.instrument .~ case atype of
              Just Equity               -> Ledger.Stock
              Just MutualFund           -> Ledger.Stock
              Just (OptionAsset _)      -> Ledger.Option
              Just (FixedIncomeAsset _) -> Ledger.Bond
              Just (CashEquivalentAsset
                    CashMoneyMarket)    -> Ledger.MoneyMarket
              Nothing                   -> error "Unexpected"
        & Ledger.symbol   .~ symbolName t
        & Ledger.quantity .~
              coerce (case t^.item.instruction of Just Sell -> -n; _ -> n)
        & Ledger.price    .~ coerce (t^.item.API.price)
        & Ledger.refs     .~ [ transactionRef t ]
    Nothing -> pure []
  where
    posts cs
        = [ post Ledger.Fees True (DollarAmount (t^.fees_.regFee))
          | t^.fees_.regFee /= 0 ]
       ++ [ post Ledger.Charges True (DollarAmount (t^.fees_.otherCharges))
          | t^.fees_.otherCharges /= 0 ]
       ++ [ post Ledger.Commissions True (DollarAmount (t^.fees_.commission))
          | t^.fees_.commission /= 0 ]

       ++ (flip Prelude.concatMap cs $ \pl ->
               [ post (fromMaybe (error $ "No account for " ++ show pl)
                                 (plAccount (pl^.plKind)))
                      False (DollarAmount (pl^.plLoss))
               | pl^.plLoss /= 0 ]
            ++ [ post act False (CommodityAmount (pl^.plLot))
                     & postMetadata %~ meta
                     & postMetadata.at "Effect" %~
                           (<|> Just (if pl^.plLoss == 0
                                      then "Opening"
                                      else "Closing")) ])

       ++ [ case t^.item.API.price of
                Just _              -> cashPost
                Nothing | isPriced  -> cashPost
                        | otherwise -> post act False NoAmount
          | case t^.item.API.price of
                Just _  -> isPriced
                Nothing -> not fromEquity ]
       ++ if | isNothing (t^.item.API.amount) || fromEquity ->
               [ post OpeningBalances False NoAmount ]
             | rounding /= 0 ->
               [ post RoundingError False (DollarAmount rounding) ]
             | otherwise -> []
      where
        rounding = - (t^.netAmount + sumLotAndPL cs)

    meta m = m
        & at "Subtype"     ?~ T.pack (show subtyp)
        & at "XId"         ?~ T.pack (show (t^.xactId))
        & at "XDate"       ?~ T.pack (iso8601Show (t^.xactDate))
        & at "Instruction" .~ t^?item.instruction._Just.to show.packed
        & at "Effect"      .~ t^?item.positionEffect._Just.to show.packed
        & at "CUSIP"       .~ t^?instrument_._Just.cusip
        & at "Instrument"  .~ t^?instrument_._Just.assetType.to assetKind
        & at "Side"        .~ t^?option'.putCall.to show.packed
        & at "Strike"      .~ t^?option'.strikePrice._Just.to thousands.packed
        & at "Expiration"  .~ t^?option'.expirationDate.to (T.pack . iso8601Show)
        & at "Contract"    .~ t^?option'.description

    act = case atype of
        Just Equity                  -> Equities actId
        Just MutualFund              -> Equities actId
        Just (OptionAsset _)         -> Options  actId
        Just (FixedIncomeAsset _)    -> Bonds actId
        Just (CashEquivalentAsset _) -> MoneyMarkets actId
        Nothing                      -> OpeningBalances

    atype      = t^?instrument_._Just.assetType
    subtyp     = t^.transactionInfo_.transactionSubType
    isPriced   = t^.netAmount /= 0 || subtyp `elem` [ OptionExpiration ]
    fromEquity = subtyp `elem` [ TransferOfSecurityOrOptionIn ]

    cashPost = post (Cash actId) False $
        if t^.netAmount == 0
        then NoAmount
        else DollarAmount (t^.netAmount)

    post a b m = Posting
        { _account      = a
        , _isVirtual    = b
        , _isBalancing  = not b
        , _amount       = m
        , _postMetadata = M.empty
        }

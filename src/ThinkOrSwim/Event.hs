{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ThinkOrSwim.Event
    ( gainsKeeper
    , GainsKeeperState(..)
    , newGainsKeeperState
    , positionEvents
    , Lot
    , shares
    , costOfShares
    , xact
    , trail
    , Event(..)
    , _OpenPosition
    , Disposition(..)
    , Transactional(..)
    , costs
    , gain
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Trans.State
import           Data.Amount
import           Data.Coerce
import           Data.Foldable
import           Data.Map (Map)
import           Data.Maybe (maybeToList)
import           Data.Split
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Utils
import           Debug.Trace
import           Prelude hiding (Float, Double, (<>))
import           Text.PrettyPrint as P
import           ThinkOrSwim.API.TransactionHistory.GetTransactions
                     (TransactionType(..), TransactionSubType(..),
                      AssetType(..), _OptionAsset)
import qualified ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import           ThinkOrSwim.Options (Options)
import qualified ThinkOrSwim.Options as Options

data Disposition
    = Long
    | Short
    deriving (Eq, Ord, Show, Enum, Bounded)

makePrisms ''Disposition

{-
Things that can happen which may change the raw sequence of transactions, or
affect the history of events having possible future implications:

- open position by buying/selling long/short
- close position with gain/loss by selling/covering, short/long-term/collectible
- adjust cost basis when opening position due to previous wash sale
- adjust cost basis of existing open position due to current wash sale
- remember fact of an unapplied wash sale loss
- equities transferred in
- shares assigned/carried away due to option assignment/exercise by counter-party
- shares bought/sold due to option exercise
- short/long-term loss/gain due to option expiration of long/short call/put
-}
data Event t
    = OpenPosition Disposition t
    | ClosePosition Disposition t t
      -- Wash Sales
    | AdjustCostBasisForOpen Disposition (Event t)
    | AdjustCostBasis Disposition (Event t) (Event t)
    | RememberWashSaleLoss Disposition (Event t)
      -- Options
    | OptionAssigned t t
    | ExerciseOption t t
      -- Transfer
    | EstablishEquityCost
        { _equityAmount :: Amount 6
        , _equityCost   :: Amount 6
        , _equityDate   :: UTCTime
        }
    -- | InterestPaid t
    -- | DividendPaid t
    | UnrecognizedTransaction t
    deriving (Eq, Ord, Show)

data EventError t
    = OpenBySellTrade t
    | OpenByCloseShortPosition t
    | CannotRenderIntoOpenEvent t
    | DispositionMismatch Disposition Disposition
    | CannotFindOpen t
    deriving (Eq, Ord, Show)

makePrisms ''Event

instance (Transactional t, Render t) => Render (EventError t) where
    rendered = \case
        OpenBySellTrade x ->
            "OpenBySellTrade"
                $$ space <> space <> rendered x
        OpenByCloseShortPosition x ->
            "OpenByCloseShortPosition"
                $$ space <> space <> rendered x
        CannotRenderIntoOpenEvent x ->
            "CannotRenderIntoOpenEvent"
                $$ space <> space <> rendered x
        DispositionMismatch x y ->
            "DispositionMismatch" <> space <> tshow x <> space <> tshow y
        CannotFindOpen x ->
            "CannotFindOpen"
                $$ space <> space <> rendered x

instance (Transactional t, Render t) => Render (Event (Lot t)) where
    rendered = \case
        OpenPosition d x ->
            "OpenPosition"
                <> space <> text (show d)
                $$ space <> space <> rendered x
        ClosePosition d x y ->
            "ClosePosition"
                <> space <> text (show d)
                $$ space <> space <> rendered x
                $$ space <> space <> rendered y
        AdjustCostBasisForOpen d x ->
            "AdjustCostBasisForOpen"
                <> space <> text (show d)
                $$ space <> space <> rendered x
        AdjustCostBasis d x y ->
            "AdjustCostBasis"
                <> space <> text (show d)
                $$ space <> space <> rendered x
                $$ space <> space <> rendered y
        RememberWashSaleLoss d x ->
            "RememberWashSaleLoss"
                <> space <> text (show d)
                $$ space <> space <> rendered x
        OptionAssigned x y ->
            "OptionAssigned"
                <> space <> rendered x
                $$ space <> rendered y
        ExerciseOption x y ->
            "ExerciseOption"
                <> space <> rendered x
                $$ space <> space <> rendered y
        EstablishEquityCost x y z ->
            "EstablishEquityCost"
                <> space <> text (show x)
                <> space <> text (show y)
                <> space <> text (show z)
        UnrecognizedTransaction t ->
            "UnrecognizedTransaction"
                $$ space <> rendered t

class Show t => Transactional t where
    ident       :: Getter t API.TransactionId
    time        :: Lens' t UTCTime
    kind        :: Getter t API.TransactionType
    subkind     :: Getter t API.TransactionSubType
    instruction :: Getter t (Maybe API.Instruction)
    symbol      :: Getter t (Maybe Text)
    cusip       :: Getter t (Maybe Text)
    underlying  :: Getter t (Maybe Text)
    asset       :: Traversal' t AssetType
    quantity    :: Lens' t (Amount 6)
    cost        :: Lens' t (Amount 6)
    fees        :: Getter t (Amount 2)
    distance    :: t -> Lens' t Integer

instance Transactional API.Transaction where
    ident          = API.xid
    time           = API.xdate
    kind           = API.xtype
    subkind        = API.xsubType
    instruction    = API.xitem.API.instruction
    cusip          = API.xcusip
    symbol         = API.xsymbol
    underlying     = API.xunderlying
    asset          = API.xasset
    quantity       = API.xamount
    cost           = API.xcost
    fees f s       = s <$ f (sumOf (API.xfees) s)
    distance t f s =
        f (abs ((t^.time.to utctDay) `diffDays` (s^.time.to utctDay)))
            <&> \x -> s & time %~
                     \(UTCTime _dy tm) ->
                         UTCTime (addDays (- x) (t^.time.to utctDay)) tm

data Lot t = Lot
    { _shares       :: Amount 6
    , _costOfShares :: Amount 6
    , _xact         :: t
    , _trail        :: [Event (Lot t)]
    }
    deriving (Eq, Ord, Show)

makeLenses ''Lot

instance Transactional t => Transactional (Lot t) where
    ident       = xact.ident
    time        = xact.time
    kind        = xact.kind
    subkind     = xact.subkind
    instruction = xact.instruction
    cusip       = xact.cusip
    symbol      = xact.symbol
    underlying  = xact.underlying
    asset       = xact.asset
    quantity    = shares
    fees        = shares `sliceOf` (xact.fees)
    cost        = costOfShares
    distance t  = xact.distance (t^.xact)

instance (Transactional t, Render t) => Render (Lot t) where
    rendered l@(Lot {..}) =
        rendered (l^.symbol.non "")
            <> space <> tshow _shares
            <> " @@ " <> tshow (l^.cost.coerced / _shares)
            <> space <> rendered _xact

sliceOf :: (Transactional t, Functor f)
        => Lens' (Lot t) (Amount 6) -> LensLike' f (Lot t) (Amount n)
        -> LensLike' f (Lot t) (Amount n)
sliceOf n l f t = t & l.percent (t^.n / t^.xact.quantity) %%~ f

mkLot :: Transactional t => t -> Lot t
mkLot t = Lot
    { _shares       = t^.quantity
    , _costOfShares = t^.cost
    , _xact         = t
    , _trail        = []
    }

alignLots :: (Transactional a, Transactional b)
          => a -> b -> (Split a, Split b)
alignLots x y
    | xq == 0 && yq == 0 = ( None x, None y )
    | xq == 0  = ( None x, All  y )
    | yq == 0  = ( All  x, None y )
    | xq == yq = ( All  x, All  y )
    | xq <  yq =
        ( All x
        , Some (y & quantity .~ xq
                  & cost     .~ xq * ycps)
               (y & quantity .~ diff
                  & cost     .~ diff * ycps)
        )
    | otherwise =
        ( Some (x & quantity .~ yq
                  & cost     .~ yq * xcps)
               (x & quantity .~ diff
                  & cost     .~ diff * xcps)
        , All y
        )
  where
    xq   = x^.quantity
    yq   = y^.quantity
    xcps = x^.cost / xq
    ycps = y^.cost / yq
    diff = abs (xq - yq)

data StateChange t = StateChange
    { _leftover     :: Maybe t
    , _results      :: [Event t]
    , _replacements :: [Event t]
    , _newEvents    :: [Event t]
    }
    deriving (Eq, Ord, Show)

makeLenses ''StateChange

newStateChange :: t -> StateChange t
newStateChange t = StateChange
    { _leftover     = Just t
    , _results      = []
    , _replacements = []
    , _newEvents    = []
    }

type EventState t a =
    StateT [Event (Lot t)] (Except (EventError (Lot t))) a

isOption :: Transactional t => t -> Bool
isOption = has (asset._OptionAsset)

isOptionAssignment :: Transactional t => t -> Bool
isOptionAssignment t = t^.subkind == OptionAssignment

intoEvent :: Transactional t => Lot t -> EventState t (Event (Lot t))
intoEvent t = case t^.subkind of
    BuyTrade           -> pure $ OpenPosition Long t
    SellTrade
        | isOption t   -> pure $ OpenPosition Short t
        | otherwise    -> throwError $ OpenBySellTrade t
    ShortSale          -> pure $ OpenPosition Short t
    CloseShortPosition -> throwError $ OpenByCloseShortPosition t
    TransferIn         -> pure $ OpenPosition Long t
    OptionAssignment   ->
        pure $ OpenPosition (if t^.cost < 0 then Long else Short) t
    _                  -> throwError $ CannotRenderIntoOpenEvent t

foldEvents :: (Transactional t, Render t)
           => Lot t
           -> (Event (Lot t) -> Event (Lot t))
           -> (Lot t -> Event (Lot t) -> EventState t (StateChange (Lot t)))
           -> EventState t [Event (Lot t)]
foldEvents t g k = do
    events <- use id
    sc <- (\f -> foldlM f (newStateChange t) events) $
        \sc e -> case sc^.leftover of
            Nothing ->
                pure $ sc & replacements <>~ [e]
            Just t' -> do
                sc' <- k t' e
                pure $ sc
                     & leftover      .~ sc'^.leftover
                     & results      <>~ sc'^.results
                     & replacements <>~ sc'^.replacements
                     & newEvents    <>~ sc'^.newEvents

    -- jww (2020-05-04): intoEvent should only be called for newly opened
    -- positions.
    traceM $ render $ "sc^.leftover = " <> rendered (sc^.leftover)
    left <- traverse intoEvent (sc^.leftover)
    traceM $ render $ "left = " <> rendered left
    let res = map g (maybeToList left)
    traceM $ render $ "res = " <> rendered res
    id .= sc^.replacements ++ sc^.newEvents ++ res
    pure $ res ++ map g (sc^.results)

openPosition :: (Transactional t, Render t)
             => Disposition -> Lot t -> EventState t [Event (Lot t)]
openPosition disp t = foldEvents t (addFees t) $ \u e -> case e of
    EstablishEquityCost amt cst dt
        | not (isOption t || isOptionAssignment t) -> do
        nev <- intoEvent $
            if amt < u^.quantity
            then u & quantity .~ amt
                   & cost     .~ cst
                   & time     .~ dt
            else u & cost +~ cst^.percent (u^.quantity / amt)
                   & time .~ dt
        forM_ (nev^?_OpenPosition._1) $ \ed ->
            unless (disp == ed) $
                throwError $ DispositionMismatch disp ed
        pure StateChange
            { _leftover     = if amt < u^.quantity
                              then Just (u & quantity -~ amt)
                              else Nothing
            , _results      = [nev]
            , _replacements =
                [ EstablishEquityCost
                    (amt - u^.quantity)
                    (cst^.percent ((amt - u^.quantity) / amt))
                    dt
                | amt > u^.quantity ]
            , _newEvents    = [nev]
            }

    ev@(ClosePosition ed o c)
        | -- jww (2020-05-03): This restriction shouldn't be needed, if the
          -- option could be considerable as a replacement for the equity.
          o^.symbol == t^.symbol &&
          not (isOptionAssignment t) ->
        if c^.distance o <= 30
        then do
            let (s, _d) = c `alignLots` u
                part = max 1 (u^.quantity / c^.quantity)
            pure StateChange
                { _leftover     = Just $ u
                    & cost +~ ev^.gain.percent part.coerced
                , _results      = []
                , _replacements = ClosePosition ed o
                    <$> maybeToList (s^?_SplitKept)
                , _newEvents    = []
                }
        else pure $ newStateChange u

    -- Opening against an assigned option happens only for a Put.
    OptionAssigned o c | o^.underlying == u^.symbol -> do
        let (s, _d) = (o & quantity *~ 100) `alignLots` u
            part = max 1 (u^.quantity / (o^.quantity * 100))
        pure StateChange
            { _leftover     = Just $
                u & cost +~ o^.cost.percent part.coerced
            , _results      = []
            , _replacements = OptionAssigned
                <$> maybeToList (s^?_SplitKept) <*> pure c
            , _newEvents    = []
            }

    _ -> pure StateChange
            { _leftover     = Just u
            , _results      = []
            , _replacements = [e]
            , _newEvents    = []
            }

-- Fees paid to open are borne in the cost basis of the asset
addFees :: Transactional t => Lot t -> Event (Lot t) -> Event (Lot t)
addFees t x | Just u <- x^?_OpenPosition._2 =
    trace ("id = " ++ show (u^.ident)
             ++ ", cost = " ++ show (u^.cost)
             ++ ", fees = " ++ show (t^.fees.percent (part u))) $
    (x & _OpenPosition._2.cost -~ t^.fees.coerced.percent (part u))
  where
    -- Thanks to laziness, only evaluated when needed
    part u = u^.quantity / t^.quantity
addFees _ x = x

closePosition :: (Transactional t, Render t)
              => (Event (Lot t) -> Bool) -> Lot t
              -> EventState t [Event (Lot t)]
closePosition p t = foldEvents t (addFees t) $ \u e -> case e of
    ev@(OpenPosition ed o) | p ev && o^.symbol == u^.symbol -> do
        let (s, d) = o `alignLots` u
            res = maybeToList $
                (if u^.subkind == OptionAssignment
                 then OptionAssigned
                 else ClosePosition ed)
                    <$> s^?_SplitUsed <*> d^?_SplitUsed
        pure StateChange
             { _leftover     = d^?_SplitKept
             , _results      = res
             , _replacements = s^.._SplitKept.to (OpenPosition ed)
             , _newEvents    =
                 case res of
                     [x] | u^.distance o <= 30 && x^.gain < 0 -> res
                     [x] | has _OptionAssigned x -> res
                     _ -> []
             }

    -- Closing against an assigned option happens only for a Call.
    OptionAssigned o c | o^.underlying == u^.symbol -> do
        let (s, _d) = (o & quantity *~ 100) `alignLots` u
            part = max 1 (u^.quantity / (o^.quantity * 100))
        pure StateChange
            { _leftover     = Just $ u
                & cost +~ o^.cost.percent part.coerced
            , _results      = []
            , _replacements = OptionAssigned
                <$> maybeToList (s^?_SplitKept) <*> pure c
            , _newEvents    = []
            }

    _ ->
        pure StateChange
            { _leftover     = Just u
            , _results      = []
            , _replacements = [e]
            , _newEvents    = []
            }

openIs :: Disposition -> Event (Lot t) -> Bool
openIs disp ev = ev^?!_OpenPosition._1 == disp

dispatchOnKind :: (Transactional t, Render t, Eq t)
               => Lot t
               -> (API.TransactionType, API.TransactionSubType)
               -> EventState t [Event (Lot t)]
dispatchOnKind t = \case
    -- (DividendOrInterest, AdrFee) -> pure []
    -- (DividendOrInterest, FreeBalanceInterestAdjustment) -> pure []
    -- (DividendOrInterest, OffCycleInterest) -> pure []
    -- (DividendOrInterest, SecuritiesInterestIncome) -> pure []
    -- (DividendOrInterest, ForeignTaxWithheld) -> pure []
    -- (DividendOrInterest, QualifiedDividend) -> pure []

    -- (ElectronicFund, TransferFromCashAccount) -> pure []
    -- (ElectronicFund, DirectDeposit) -> pure []
    -- (ElectronicFund, TransferToFuturesAccount) -> pure []

    -- (Journal, TransferFromFuturesAccount) -> pure []
    -- (Journal, TransferToFuturesAccount) -> pure []
    -- (Journal, MiscellaneousJournalEntry) -> pure []
    -- (Journal, MarkToMarket) -> pure []
    -- (Journal, CashAlternativesPurchase) -> pure []
    -- (Journal, Rebate) -> pure []
    -- (Journal, CashAlternativesRedemption) -> pure []
    -- (Journal, TransferFromForexAccount) -> pure []
    -- (Journal, TransferToForexAccount) -> pure []

    -- (ReceiveAndDeliver, InternalTransfer) -> pure []
    -- Option assignment is much like an expiration, except P/L is carried
    -- over to the subsequent equity transaction: the cost basis if
    -- purchasing, or the gain/loss if selling. This equity transfer is a
    -- (Trade, OptionAssignment) transaction and follows this event.
    (ReceiveAndDeliver, OptionAssignment) ->
        closePosition (openIs Short) t
    (ReceiveAndDeliver, OptionExpiration) ->
        closePosition (const True) t
    -- (ReceiveAndDeliver, CashAlternativesPurchase) -> pure []
    -- (ReceiveAndDeliver, CashAlternativesRedemption) -> pure []
    (ReceiveAndDeliver, TransferIn) ->
        openPosition Long t

    (Trade, BuyTrade) ->
        openPosition Long t
    (Trade, CloseShortPosition) ->
        closePosition (openIs Short) t
    (Trade, OptionAssignment)
        | t^.instruction == Just API.Buy -> openPosition Long t
        | otherwise -> closePosition (openIs Long) t
    -- (Trade, BondsRedemption) -> pure []
    (Trade, SellTrade) ->
        closePosition (openIs Long) t
    (Trade, ShortSale) ->
        openPosition Short t
    -- (Trade, TradeCorrection) -> pure []

    -- (WireIn, WireIncoming) -> pure []

    _ -> pure [UnrecognizedTransaction t]

    -- x -> error $ "Unrecognized transaction type+subtype: " ++ show x

processTransaction :: (Transactional t, Render t, Eq t)
                   => t -> EventState t [Event (Lot t)]
processTransaction = go . mkLot
  where go t = dispatchOnKind t (t^.kind, t^.subkind)

gain :: Transactional t => Getter (Event t) (Amount 2)
gain f s@(ClosePosition _ o c) =
    s <$ f (coerce (o^.cost + c^.cost) - c^.fees)
gain f s = s <$ f 0

costs :: Transactional t => Getter (Event t) (Amount 6)
costs f = \case
    s@(OpenPosition _ t)            -> s <$ f (totalCost t)
    s@(ClosePosition _ _ t)         -> s <$ f (totalCost t)
    s@(AdjustCostBasisForOpen _ ev) -> s <$ f (ev^.costs)
    s@(AdjustCostBasis _ _ ev)      -> s <$ f (ev^.costs)
    s@(RememberWashSaleLoss _ ev)   -> s <$ f (ev^.costs)
    s@(OptionAssigned _ t)          -> s <$ f (totalCost t)
    s@(ExerciseOption _ t)          -> s <$ f (totalCost t)
    s@(EstablishEquityCost _ c _)   -> s <$ f c
    s@(UnrecognizedTransaction t)   -> s <$ f (totalCost t)
  where
    totalCost t = t^.cost + t^.fees.coerced

data GainsKeeperState t = GainsKeeperState
    { _positionEvents :: Map Text [Event (Lot t)]
    }
    deriving (Eq, Ord, Show)

makeLenses ''GainsKeeperState

newGainsKeeperState :: GainsKeeperState t
newGainsKeeperState = GainsKeeperState
    { _positionEvents = mempty
    }

runStateExcept :: StateT s (Except e) a -> State s (Either e a)
runStateExcept (StateT f) = StateT $ \s -> do
    eres <- runExceptT (f s)
    pure $ case eres of
        Left err        -> (Left err, s)
        Right (res, s') -> (Right res, s')

gainsKeeper :: (Transactional t, Eq t, Render t)
            => Options -> t -> State (GainsKeeperState t) [Event (Lot t)]
gainsKeeper opts t =
    zoom (positionEvents.at (t^.underlying.non "").non []) $ do
        hist <- use id
        eres <- runStateExcept (processTransaction t)
        let doc = ""
                $$ "xct -> " <> rendered (t^.symbol.non "")
                             <> space <> rendered t
                             <> space <> "@@"
                             <> space <> tshow (if t^.quantity == 0
                                                then 0
                                                else t^.cost / t^.quantity)
                $$ "" $$ "bef -> " <> rendered hist
        case eres of
            Left err  -> do
                when (matches t) $
                    renderM $ doc
                        $$ "" $$ "err => " <> rendered err
                        $$ "" $$ "--------------------------------------------------"
                pure []
            Right res -> do
                hist' <- use id
                when (matches t) $
                    renderM $ doc
                        $$ "" $$ "aft => " <> rendered hist'
                        $$ "" $$ "res => " <> rendered res
                        $$ "" $$ "--------------------------------------------------"
                pure res
  where
    matches x =
        opts^.Options.traceAll
      || case opts^.Options.traceSymbol of
            Nothing  -> False
            Just sym -> x^.symbol == Just (T.pack sym)
      || case opts^.Options.traceUnderlying of
            Nothing  -> False
            Just sym -> x^.underlying == Just (T.pack sym)
      || case opts^.Options.traceId of
            Nothing  -> False
            Just i   -> show (x^.ident) == i

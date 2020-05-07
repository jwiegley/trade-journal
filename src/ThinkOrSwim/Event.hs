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
    , isCall
    , isOption
    , isOptionAssignment
    ) where

import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Trans.State
import           Data.Amount
import           Data.Foldable
import           Data.Map (Map)
import           Data.Maybe (maybeToList)
import           Data.Split
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Utils
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
      -- Wash Sales and P/L
    | AdjustCostBasisForOpen Disposition (Event t) (Amount 6)
    | AdjustCostBasis Disposition (Event t) (Event t)
    | RememberWashSaleLoss Disposition (Event t)
    | CapitalGain Disposition (Amount 6) (Event t)
    | CapitalLoss Disposition (Amount 6) (Event t)
      -- Options
    | OptionAssigned t t
    | OptionExercised t t
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
                <> space <> tshow d
                $$ space <> space <> rendered x
        ClosePosition d x y ->
            "ClosePosition"
                <> space <> tshow d
                $$ space <> space <> rendered x
                $$ space <> space <> rendered y
        AdjustCostBasisForOpen d x y ->
            "AdjustCostBasisForOpen"
                <> space <> tshow d
                $$ space <> space <> rendered x
                <> space <> " ==> " <> space <> rendered y
        AdjustCostBasis d x y ->
            "AdjustCostBasis"
                <> space <> tshow d
                $$ space <> space <> rendered x
                $$ space <> space <> rendered y
        CapitalGain d g x ->
            "CapitalGain"
                <> space <> tshow d
                $$ space <> space <> tshow g
                $$ space <> space <> rendered x
        CapitalLoss d g x ->
            "CapitalLoss"
                <> space <> tshow d
                $$ space <> space <> tshow g
                $$ space <> space <> rendered x
        RememberWashSaleLoss d x ->
            "RememberWashSaleLoss"
                <> space <> tshow d
                $$ space <> space <> rendered x
        OptionAssigned x y ->
            "OptionAssigned"
                <> space <> rendered x
                $$ space <> rendered y
        OptionExercised x y ->
            "OptionExercised"
                <> space <> rendered x
                $$ space <> space <> rendered y
        EstablishEquityCost x y z ->
            "EstablishEquityCost"
                <> space <> tshow x
                <> space <> tshow y
                <> space <> tshow z
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
    rendered l | l^.shares == 0 =
        rendered (l^.symbol.non "")
            <> space <> rendered (l^.xact)
    rendered l@(Lot {..}) =
        rendered (l^.symbol.non "")
            <> space <> tshow _shares
            <> " @@ " <> tshow (l^.cost.coerced / _shares)
            <> " + " <> tshow (l^.fees)
            <> space <> rendered _xact

sliceOf :: (Transactional t, Functor f)
        => Lens' (Lot t) (Amount 6) -> LensLike' f (Lot t) (Amount n)
        -> LensLike' f (Lot t) (Amount n)
sliceOf _ _ f t | t^.xact.quantity == 0 = t <$ f 0
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
    { _results      :: [Event t]
    , _replacements :: [Event t]
    , _newEvents    :: [Event t]
    }
    deriving (Eq, Ord, Show)

makeLenses ''StateChange

newStateChange :: StateChange t
newStateChange = StateChange
    { _results      = []
    , _replacements = []
    , _newEvents    = []
    }

type EventStateT s t a = StateT s (Except (EventError t)) a

type HistoryState t a = EventStateT [Event t] t a

type ChangeState t a = EventStateT (StateChange t) t a

isOption :: Transactional t => t -> Bool
isOption = has (asset._OptionAsset)

isOptionAssignment :: Transactional t => t -> Bool
isOptionAssignment t = t^.subkind == OptionAssignment

isCall :: Transactional t => t -> Bool
isCall t = t^?asset._OptionAsset.API.putCall == Just API.Call

-- Fees paid to open are borne in the cost basis of the asset
addFees :: Transactional t => Lot t -> Event (Lot t) -> Event (Lot t)
addFees t x | Just u <- x^?_OpenPosition._2 =
    (x & _OpenPosition._2.cost -~ t^.fees.coerced.percent (part u))
  -- Thanks to laziness, only evaluated when needed
  where part u = u^.quantity / t^.quantity
addFees _ x = x

intoEvent :: (MonadError (EventError (Lot t)) m, Transactional t)
          => Lot t -> m (Event (Lot t))
intoEvent t = case t^.subkind of
    BuyTrade           -> pure $ OpenPosition Long t
    SellTrade
        | isOption t   -> pure $ OpenPosition Short t
        | otherwise    -> throwError $ OpenBySellTrade t
    ShortSale          -> pure $ OpenPosition Short t
    CloseShortPosition -> throwError $ OpenByCloseShortPosition t
    TransferIn         -> pure $ OpenPosition Long t
    _                  -> throwError $ CannotRenderIntoOpenEvent t

foldEvents :: (Transactional t, Render t)
           => Lot t
           -> (Lot t -> Maybe (Event (Lot t))
                    -> ChangeState (Lot t) (Maybe (Lot t)))
           -> HistoryState (Lot t) [Event (Lot t)]
foldEvents t k = do
    events <- use id
    (ml, sc) <- lift $ flip runStateT newStateChange $
        (\f -> foldlM f (Just t) (justify events)) $
            \ml me -> case ml of
                Nothing -> Nothing <$ case me of
                    Just e  -> replacements <>= [e]
                    Nothing -> pure ()
                Just t' -> k t' me
    when (has _Just ml) $
        error $ "foldEvents: unexpected remainder: " ++ show ml
    id .= sc^.replacements ++ sc^.newEvents
    pure $ sc^.results

openPosition :: (Transactional t, Render t)
             => Disposition -> Lot t -> HistoryState (Lot t) [Event (Lot t)]
openPosition disp t = foldEvents t $ \u me -> case me of
    Just (EstablishEquityCost amt cst dt)
        | not (isOption t || isOptionAssignment t) -> do
        nev <- fmap (addFees t) $ intoEvent $
            if amt < u^.quantity
            then u & quantity .~ amt
                   & cost     .~ cst
                   & time     .~ dt
            else u & cost +~ cst^.percent (u^.quantity / amt)
                   & time .~ dt

        forM_ (nev^?_OpenPosition._1) $ \ed ->
            unless (disp == ed) $
                throwError $ DispositionMismatch disp ed

        results      <>= [nev]
        newEvents    <>= [nev]
        replacements <>=
            [ EstablishEquityCost
                (amt - u^.quantity)
                (cst^.percent ((amt - u^.quantity) / amt))
                dt
            | amt > u^.quantity ]

        pure $ if amt < u^.quantity
               then Just (u & quantity -~ amt)
               else Nothing

    Just ev@(ClosePosition ed o c)
        | -- jww (2020-05-03): This restriction shouldn't be needed, if the
          -- option could be considered as a replacement for the equity.
          o^.symbol == t^.symbol ->
        if c^.distance o <= 30
        then do
            let (s, d) = c `alignLots` u
                adj    = (s^?!_SplitUsed.cost + d^?!_SplitUsed.cost)^.coerced
            replacements
                <>= (ClosePosition ed o <$> maybeToList (s^?_SplitKept))
            results <>= [ AdjustCostBasisForOpen disp ev adj ]
            -- We wash three failing closes by adding all of that to the cost
            -- basis of the opening transaction. Thus, we generate three
            -- instances of AdjustCostBasisForOpen, but only one OpenPosition.
            pure $ Just $ u & cost +~ adj
        else
            pure $ Just u

    -- Opening against an assigned option happens only for a Put.
    Just ev@(OptionAssigned o c)
        | o^.underlying == u^.symbol && isOptionAssignment u -> do
        let (s, d) = (o & quantity *~ 100) `alignLots` u
            res    = d ^.. _SplitUsed
                         . to (cost +~ s^?!_SplitUsed.cost)
                         . to (assert (not (isCall o)) $ OpenPosition Long)
                         . to (addFees t)
            dur | u^.distance o > 365 = Long
                | otherwise = Short
        results      <>= [ CapitalGain dur (negate (s^?!_SplitUsed.cost)) ev ]
                      ++ res
        newEvents    <>= res
        replacements <>= (OptionAssigned <$> maybeToList (s^?_SplitKept)
                                         <*> pure c)
        pure $ d^?_SplitKept

    Just e -> do
        replacements <>= [e]
        pure $ Just u

    Nothing -> do
        nev <- addFees t <$> intoEvent u
        results   <>= [nev]
        newEvents <>= [nev]
        pure Nothing

closePosition :: (Transactional t, Render t)
              => (Event (Lot t) -> Bool) -> Lot t
              -> HistoryState (Lot t) [Event (Lot t)]
closePosition p t = foldEvents t $ \u me -> case me of
    Just ev@(OpenPosition ed o) | p ev && o^.symbol == u^.symbol -> do
        let (s, d) = o `alignLots` u
        forM_ ((,) <$> s^?_SplitUsed <*> d^?_SplitUsed) $ \(su, du) -> do
            let res | isOptionAssignment u = []
                    | otherwise = [ ClosePosition ed su du ]
                adj = s^?!_SplitUsed.cost +
                      d^?!_SplitUsed.cost - d^?!_SplitUsed.fees.coerced
                typ | adj > 0   = CapitalGain
                    | otherwise = CapitalLoss
                dur | du^.distance su > 365 = Long
                    | otherwise             = Short
            results      <>= [ typ dur (adj^.coerced) ev ] ++ res
            replacements <>= s^.._SplitKept.to (OpenPosition ed)
            newEvents    <>= if u^.distance o <= 30 && adj < 0 then res else []
        pure $ if isOptionAssignment u
               then Just u
               else d^?_SplitKept

    -- Closing against an assigned option happens only for a Call. If it was a
    -- covered call, then the case above for matching against an OpenPosition
    -- will have handled it.
    --
    -- NOTE: Unlike the Put case, this code is only used for assigned calls if
    -- the assignment does not fully close existing equity positions.
    Just ev@(OptionAssigned o c)
        | o^.underlying == u^.symbol && isOptionAssignment u -> do
        let (s, d) = (o & quantity *~ 100) `alignLots` u
            res    = d ^.. _SplitUsed
                         . to (cost +~ s^?!_SplitUsed.cost)
                         . to (assert (isCall o) $ OpenPosition Short)
                         . to (addFees t)
            dur | u^.distance o > 365 = Long
                | otherwise           = Short
        results      <>= [ CapitalGain dur (s^?!_SplitUsed.cost) ev ]
                      ++ res
        newEvents    <>= res
        replacements <>= (OptionAssigned <$> maybeToList (s^?_SplitKept)
                                         <*> pure c)
        pure $ d^?_SplitKept

    Just e -> do
        replacements <>= [e]
        pure $ Just u

    Nothing -> do
        nev <- addFees t <$> intoEvent u
        results   <>= [nev]
        newEvents <>= [nev]
        pure Nothing

openIs :: Disposition -> Event (Lot t) -> Bool
openIs disp ev = ev^?!_OpenPosition._1 == disp

dispatchOnKind :: (Transactional t, Render t, Eq t)
               => Lot t
               -> (API.TransactionType, API.TransactionSubType)
               -> HistoryState (Lot t) [Event (Lot t)]
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
    (ReceiveAndDeliver, CashAlternativesPurchase) ->
        pure [ UnrecognizedTransaction $ t & cost .~ t^.quantity ]
    (ReceiveAndDeliver, CashAlternativesRedemption) ->
        pure [ UnrecognizedTransaction $ t & cost .~ t^.quantity ]
    (ReceiveAndDeliver, TransferIn) ->
        openPosition Long t

    (Trade, BuyTrade) ->
        openPosition Long t
    (Trade, CloseShortPosition) ->
        closePosition (openIs Short) t
    (Trade, OptionAssignment)
        | t^.instruction == Just API.Buy ->
            openPosition Long t
        | otherwise ->
            -- jww (2020-05-05): A naked call assigns short stock
            closePosition (openIs Long) t
    (Trade, BondsRedemption) ->
        pure [ UnrecognizedTransaction $ t & cost %~ negate ]
    (Trade, SellTrade) ->
        closePosition (openIs Long) t
    (Trade, ShortSale) ->
        openPosition Short t
    -- (Trade, TradeCorrection) -> pure []

    -- (WireIn, WireIncoming) -> pure []

    _ -> pure [ UnrecognizedTransaction t ]

    -- x -> error $ "Unrecognized transaction type+subtype: " ++ show x

processTransaction :: (Transactional t, Render t, Eq t)
                   => t -> HistoryState (Lot t) [Event (Lot t)]
processTransaction = go . mkLot
  where go t = dispatchOnKind t (t^.kind, t^.subkind)

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

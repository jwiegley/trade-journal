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
import           Data.List (tails)
import           Data.Map (Map)
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
    | ClosePosition (Maybe Disposition) t
    | PositionClosed Disposition t t
      -- Wash Sales and P/L
    | WashLossApplied Disposition (Event t) (Amount 6)
    | AdjustCostBasis Disposition (Event t) (Event t)
    | RememberingWashSaleLoss Disposition (Event t)
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

instance (Transactional t, Render t) => Render (Event t) where
    rendered = \case
        OpenPosition d x ->
            "OpenPosition"
                <> space <> tshow d
                $$ space <> space <> rendered x
        ClosePosition d x ->
            "ClosePosition"
                <> space <> tshow d
                $$ space <> space <> rendered x
        PositionClosed d x y ->
            "PositionClosed"
                <> space <> tshow d
                $$ space <> space <> rendered x
                $$ space <> space <> rendered y
        WashLossApplied d x y ->
            "WashLossApplied"
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
        RememberingWashSaleLoss d x ->
            "RememberingWashSaleLoss"
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

data StateChange t
    = Result (Event t)
    | ReplaceEvent (Event t)
    | AddEvent (Event t)
    deriving (Eq, Ord, Show)

makePrisms ''StateChange

instance (Transactional t, Render t) => Render (StateChange t) where
    rendered = \case
        Result e       -> "Result "       <> rendered e
        ReplaceEvent e -> "ReplaceEvent " <> rendered e
        AddEvent e     -> "AddEvent "     <> rendered e

type EventStateT s t a = StateT s (Except (EventError t)) a

type HistoryState t a = EventStateT [Event t] t a

type ChangeState t a = EventStateT [StateChange t] t a

isOption :: Transactional t => t -> Bool
isOption = has (asset._OptionAsset)

isOptionAssignment :: Transactional t => t -> Bool
isOptionAssignment t = t^.subkind == OptionAssignment

isCall :: Transactional t => t -> Bool
isCall t = t^?asset._OptionAsset.API.putCall == Just API.Call

-- Fees paid to open are borne in the cost basis of the asset
addFees :: Transactional t => Event t -> Event t
addFees x | Just u <- x^?_OpenPosition._2 =
    (x & _OpenPosition._2.cost -~ u^.fees.coerced)
addFees x = x

intoEvent :: (MonadError (EventError t) m, Transactional t)
          => t -> m (Event t)
intoEvent t = case t^.subkind of
    BuyTrade           -> pure $ OpenPosition Long t
    SellTrade
        | isOption t   -> pure $ OpenPosition Short t
        | otherwise    -> throwError $ OpenBySellTrade t
    ShortSale          -> pure $ OpenPosition Short t
    CloseShortPosition -> throwError $ OpenByCloseShortPosition t
    TransferIn         -> pure $ OpenPosition Long t
    _                  -> throwError $ CannotRenderIntoOpenEvent t

withEvents :: [Event t] -> HistoryState t a -> Except (EventError t) a
withEvents = flip evalStateT

foldEvents :: (Transactional t, Render t, Render a)
           => a
           -> (a -> [Event t] -> ChangeState t (Maybe a))
           -> HistoryState t [Event t]
foldEvents t k = do
    evs <- use id
    (ml, sc) <- lift $ flip runStateT [] $
        (\f -> foldlM f (Just t) (tails evs)) $
            \ml es -> case ml of
                Nothing -> Nothing <$ case es of
                    []    -> pure ()
                    (e:_) -> change $ ReplaceEvent e
                Just t' -> k t' es
    when (has _Just ml) $
        error $ "foldEvents: unexpected remainder: "
            ++ render (rendered ml)
    id .= sc^..each._ReplaceEvent ++ sc^..each._AddEvent
    pure $ sc^..each._Result

applyEvent :: (Transactional t, Render t)
           => Event t -> HistoryState t [Event t]
applyEvent = foldEvents ?? (flip handleEvent)

changes :: [StateChange t] -> ChangeState t ()
changes = (id <>=)

change :: StateChange t -> ChangeState t ()
change = changes . (:[])

eventFromTransaction :: (Transactional t, Render t, Eq t) => t -> Event t
eventFromTransaction t = case (t^.kind, t^.subkind) of
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
        ClosePosition (Just Short) t
    (ReceiveAndDeliver, OptionExpiration) ->
        ClosePosition Nothing t
    (ReceiveAndDeliver, CashAlternativesPurchase) ->
        UnrecognizedTransaction $ t & cost .~ t^.quantity
    (ReceiveAndDeliver, CashAlternativesRedemption) ->
        UnrecognizedTransaction $ t & cost .~ t^.quantity
    (ReceiveAndDeliver, TransferIn) ->
        OpenPosition Long t

    (Trade, BuyTrade) ->
        OpenPosition Long t
    (Trade, CloseShortPosition) ->
        ClosePosition (Just Short) t
    (Trade, OptionAssignment)
        | t^.instruction == Just API.Buy ->
            OpenPosition Long t
        | otherwise ->
            -- jww (2020-05-05): A naked call assigns short stock
            ClosePosition (Just Long) t
    (Trade, BondsRedemption) ->
        UnrecognizedTransaction $ t & cost %~ negate
    (Trade, SellTrade) ->
        ClosePosition (Just Long) t
    (Trade, ShortSale) ->
        OpenPosition Short t
    -- (Trade, TradeCorrection) -> pure []

    -- (WireIn, WireIncoming) -> pure []

    _ -> UnrecognizedTransaction t

    -- x -> error $ "Unrecognized transaction type+subtype: " ++ show x

handleEvent :: (Transactional t, Render t)
            => [Event t]        -- ^ historical events
            -> Event t          -- ^ current event
            -> ChangeState t (Maybe (Event t))
              -- ^ recorded change, and what remains
handleEvent (EstablishEquityCost amt cst dt:_) ev@(OpenPosition disp o)
    | not (isOption o || isOptionAssignment o) = do
    nev <- fmap addFees $ intoEvent $
        if amt < o^.quantity
        then o & quantity .~ amt
               & cost     .~ cst
               & time     .~ dt
        else o & cost     +~ cst^.percent (o^.quantity / amt)
               & time     .~ dt

    forM_ (nev^?_OpenPosition._1) $ \ed -> unless (disp == ed) $
        throwError $ DispositionMismatch disp ed

    change $ AddEvent nev
    change $ Result nev
    when (amt > quant) $
        change $ ReplaceEvent (EstablishEquityCost
                                 (amt - quant)
                                 (cst^.percent ((amt - quant) / amt))
                                 dt)

    pure $ if amt < quant
           then Just $ ev & _OpenPosition._2.quantity -~ amt
           else Nothing
  where
    quant = o^.quantity

handleEvent (ev@(PositionClosed ed o c):_) opos@(OpenPosition disp u)
    | -- jww (2020-05-03): This restriction shouldn't be needed, if the
      -- option might be considered as a replacement for the equity.
      o^.symbol == u^.symbol =
    if c^.distance o <= 30
    then do
        let (s, _) = c `alignLots` u
            adj    = (o^.cost.percent
                        (s^?!_SplitUsed.quantity / o^.quantity) +
                      s^?!_SplitUsed.cost -
                      s^?!_SplitUsed.fees.coerced)^.coerced

        changes $ ReplaceEvent . PositionClosed ed o <$> s^.._SplitKept
        change $ Result (WashLossApplied disp ev adj)

        -- We wash failing closes by adding the amount to the cost basis of
        -- the opening transaction. Thus, we generate three instances of
        -- WashLossApplied, but only one OpenPosition.
        pure $ Just $ opos & _OpenPosition._2.cost +~ adj
    else
        -- This drops the PositionClosed from the history.
        pure $ Just opos

-- Opening against an assigned option happens only for a Put.
handleEvent (ev@(OptionAssigned o c):_) (OpenPosition disp u)
    | o^.underlying == u^.symbol && isOptionAssignment u = do
    let (s, d) = (o & quantity *~ 100) `alignLots` u
        res    = d ^.. _SplitUsed
                     . to (cost +~ s^?!_SplitUsed.cost)
                     . to (assert (not (isCall o)) $ OpenPosition Long)
                     . to addFees
        dur | u^.distance o > 365 = Long
            | otherwise = Short

    changes $ Result
        <$> ([ CapitalGain dur (negate (s^?!_SplitUsed.cost)) ev ] ++ res)
    changes $ AddEvent <$> res
    changes $ ReplaceEvent <$> (OptionAssigned <$> s^.._SplitKept <*> pure c)

    pure $ d^?_SplitKept.to (OpenPosition disp)

handleEvent (ev@(OpenPosition ed o):evs) cp@(ClosePosition mdisp u)
    | case mdisp of Just disp -> ed == disp; Nothing -> True,
      o^.symbol == u^.symbol = do
    let (s, d) = o `alignLots` u
    forM_ ((,) <$> s^?_SplitUsed <*> d^?_SplitUsed) $ \(su, du) -> do
        let res | isOptionAssignment u = []
                | otherwise = [ PositionClosed ed su du ]
            adj = s^?!_SplitUsed.cost +
                  d^?!_SplitUsed.cost - d^?!_SplitUsed.fees.coerced
            typ | adj > 0   = CapitalGain
                | otherwise = CapitalLoss
            dur | du^.distance su > 365 = Long
                | otherwise             = Short

        changes $ Result <$> ([ typ dur (adj^.coerced) ev ] ++ res)
        changes $ ReplaceEvent <$> s^.._SplitKept.to (OpenPosition ed)

        -- After closing at a loss, and if the loss occurs within 30 days
        -- of its corresponding open, and there is another open within 30
        -- days of the loss, close it and re-open so it's repriced by the
        -- wash loss.
        when (u^.distance o <= 30 && adj < 0) $ do
            inner <- lift $ withEvents (reverse evs) $
                foldEvents u $ \u' -> \case
                    (OpenPosition ed' o':_)
                        | ed == ed' && o'^.symbol == u'^.symbol -> do
                          let (s', d') = o' `alignLots` u'
                          changes $ Result <$>
                            (s'^.._SplitUsed.to (PositionClosed ed' o') ++
                             s'^.._SplitUsed.to (OpenPosition ed'))
                          pure $ d'^?_SplitKept
                    [] -> pure Nothing
                    _  -> pure $ Just u'

            renderM $ "inner: " <> rendered inner
            -- jww (2020-05-07): reprice (inner^.results)
            -- jww (2020-05-08): Record the part unused as
            -- RememberingWashSaleLoss, and render it to Ledger as a comment
            -- on the transaction.
            -- jww (2020-05-08): Only add back what remains unhandled.
            changes $ AddEvent <$> res

    pure $ if isOptionAssignment u
           then Just cp
           else d^?_SplitKept.to (ClosePosition mdisp)

-- Closing against an assigned option happens only for a Call. If it was a
-- covered call, then the case above for matching against an OpenPosition will
-- have handled it.
--
-- NOTE: Unlike the Put case, this code is only used for assigned calls if the
-- assignment does not fully close existing equity positions.
handleEvent (ev@(OptionAssigned o c):_) (ClosePosition disp u)
    | o^.underlying == u^.symbol && isOptionAssignment u = do
    let (s, d) = (o & quantity *~ 100) `alignLots` u
        res    = d ^.. _SplitUsed
                     . to (cost +~ s^?!_SplitUsed.cost)
                     . to (assert (isCall o) $ OpenPosition Short)
                     . to addFees
        dur | u^.distance o > 365 = Long
            | otherwise           = Short

    changes $ Result <$> [ CapitalGain dur (s^?!_SplitUsed.cost) ev ] ++ res
    changes $ AddEvent <$> res
    changes $ ReplaceEvent <$> (OptionAssigned <$> s^.._SplitKept <*> pure c)

    pure $ d^?_SplitKept.to (ClosePosition disp)

handleEvent (e:_) u = do
    change $ ReplaceEvent e
    pure $ Just u

handleEvent [] u = do
    let r = addFees u
    change $ AddEvent r
    change $ Result r
    pure Nothing

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
    processTransaction = applyEvent . eventFromTransaction . mkLot

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

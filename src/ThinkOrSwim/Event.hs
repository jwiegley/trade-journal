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
    , item
    , trail
    , Event(..)
    , _OpenPosition
    , _PositionClosed
    , _OptionAssigned
    , Disposition(..)
    , Priced(..)
    , Transactional(..)
    , isCall
    , isOption
    , isOptionAssignment
    , Eligibility(..)
    , Applicability(..)
    , renderRefList
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Trans.State
import           Data.Amount
import           Data.Foldable
import           Data.Int (Int64)
import           Data.List (tails, isInfixOf, intersperse)
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

invertDisposition :: Disposition -> Disposition
invertDisposition Long = Short
invertDisposition Short = Long

data Eligibility
    = WashSaleEligible
    | WashSaleIneligible
    deriving (Eq, Ord, Show, Enum, Bounded)

makePrisms ''Eligibility

data Applicability
    = Deferred
    | Transferred
    | Immediate
    deriving (Eq, Ord, Show, Enum, Bounded)

makePrisms ''Applicability

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
    = OpenPosition Disposition Eligibility t
    | ClosePosition (Maybe Disposition) t
    | OpenOrClosePosition Disposition t
    | PositionClosed Disposition t t
    -- P/L and Wash Sales
    | CapitalGain Disposition (Amount 6) (Event t)
    | CapitalLoss Disposition (Amount 6) (Event t)
    -- | AdjustCostBasis Disposition (Event t) (Event t)
    | WashSale Applicability (Lot (Event t))
      -- ^ Use 'Lot (Event t)' rather than '(Amount 6) (Event t)' to
      --   support easy splitting of recorded wash sales.
    -- Options
    | OptionAssigned (Maybe t) t
    -- | OptionExercised t t
      -- Transfer
    | EstablishEquityCost
        { _equityAmount :: Amount 6
        , _equityCost   :: Amount 6
        , _equityDate   :: UTCTime
        }
    | SplitTransaction API.TransactionId [Amount 6]
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
        OpenPosition d e x ->
            "OpenPosition"
                <> space <> tshow d
                <> space <> tshow e
                $$ space <> space <> rendered x
        ClosePosition d x ->
            "ClosePosition"
                <> space <> tshow d
                $$ space <> space <> rendered x
        OpenOrClosePosition d x ->
            "OpenOrClosePosition"
                <> space <> tshow d
                $$ space <> space <> rendered x
        PositionClosed d x y ->
            "PositionClosed"
                <> space <> tshow d
                $$ space <> space <> rendered x
                $$ space <> space <> rendered y
        WashSale a x ->
            "WashSale"
                <> space <> tshow a
                $$ space <> space <> rendered x
        -- AdjustCostBasis d x y ->
        --     "AdjustCostBasis"
        --         <> space <> tshow d
        --         $$ space <> space <> rendered x
        --         $$ space <> space <> rendered y
        CapitalGain d g x ->
            "CapitalGain"
                <> space <> tshow d
                $$ space <> space <> tshow g
                $$ space <> space <> rendered x
        CapitalLoss d g x ->
            "CapitalLoss"
                <> space <> tshow d
                <> space <> tshow g
                $$ space <> space <> rendered x
        OptionAssigned x y ->
            "OptionAssigned"
                <> space <> rendered x
                $$ space <> rendered y
        -- OptionExercised x y ->
        --     "OptionExercised"
        --         <> space <> rendered x
        --         $$ space <> space <> rendered y
        EstablishEquityCost x y z ->
            "EstablishEquityCost"
                <> space <> tshow x
                <> space <> tshow y
                <> space <> tshow z
        SplitTransaction x xs ->
            "SplitTransaction"
                <> space <> tshow x
                <> space <> renderList tshow xs
        UnrecognizedTransaction t ->
            "UnrecognizedTransaction"
                $$ space <> rendered t

class Priced t where
    quantity :: Lens' t (Amount 6)
    cost     :: Lens' t (Amount 6)

class (Priced t, Show t) => Transactional t where
    ident       :: Getter t API.TransactionId
    time        :: Lens' t UTCTime
    kind        :: Getter t API.TransactionType
    subkind     :: Getter t API.TransactionSubType
    instruction :: Getter t (Maybe API.Instruction)
    effect      :: Getter t (Maybe API.PositionEffect)
    symbol      :: Getter t (Maybe Text)
    cusip       :: Getter t (Maybe Text)
    underlying  :: Getter t (Maybe Text)
    asset       :: Traversal' t AssetType
    fees        :: Getter t (Amount 2)
    distance    :: t -> Lens' t Integer

instance Priced API.Transaction where
    quantity = API.xamount
    cost     = API.xcost

instance Transactional API.Transaction where
    ident          = API.xid
    time           = API.xdate
    kind           = API.xtype
    subkind        = API.xsubType
    instruction    = API.xinstruction
    effect         = API.xeffect
    cusip          = API.xcusip
    symbol         = API.xsymbol
    underlying     = API.xunderlying
    asset          = API.xasset
    fees f s       = s <$ f (sumOf (API.xfees) s)
    distance t f s =
        f (abs ((t^.time.to utctDay) `diffDays` (s^.time.to utctDay)))
            <&> \x -> s & time %~
                     \(UTCTime _dy tm) ->
                         UTCTime (addDays (- x) (t^.time.to utctDay)) tm

data RefType
    = WashSaleRule (Amount 6)
      -- ^ A wash sale rule increases the cost basis of an equity purchase by
      --   adding previous capital losses, taking those losses off the books.

    | RollingOrder (Amount 6)
      -- ^ In a rolling order, the closing of one option is followed by the
      --   opening of another, and any credit or debit is carried across.
      --
      --   NOTE: GainsKeeper does not do this, and records one as an immediate
      --   loss/gain
    | OpeningOrder
    | ExistingEquity
    deriving (Eq, Ord, Show)

data Ref = Ref
    { _refType :: RefType
    , _refId   :: API.TransactionId
    }
    deriving (Eq, Ord, Show)

instance Render Ref where
    rendered Ref {..} = case _refType of
        WashSaleRule wash ->
            "W$" <> tshow wash <> "/" <> tshow _refId
        RollingOrder roll ->
            "R$" <> tshow roll <> "/" <> tshow _refId
        OpeningOrder   -> "" <> tshow _refId
        ExistingEquity -> "Equity"

transactionRef :: Transactional t => t -> Ref
transactionRef t = Ref OpeningOrder (t^.ident)

renderRefList :: [Ref] -> Doc
renderRefList = mconcat . intersperse "," . map rendered

data Lot t = Lot
    { _shares       :: Amount 6
    , _costOfShares :: Amount 6
    , _item         :: t
    , _trail        :: [Ref]
    }
    deriving (Eq, Ord, Show)

makePrisms ''RefType
makeLenses ''Ref
makeLenses ''Lot
makePrisms ''Event
makePrisms ''EventError

washSale :: Transactional t => Event t -> Lot (Event t)
washSale ev@(PositionClosed _ o c) = Lot
    { _shares       = c^.quantity
    , _costOfShares = o^.cost + c^.cost - c^.fees.coerced
    , _item         = ev
    , _trail        = []
    }
washSale ev = error $ "Cannot apply wash sale to: " ++ show ev

instance Priced (Lot t) where
    quantity    = shares
    cost        = costOfShares

instance Transactional t => Transactional (Lot t) where
    ident       = item.ident
    time        = item.time
    kind        = item.kind
    subkind     = item.subkind
    instruction = item.instruction
    effect      = item.effect
    cusip       = item.cusip
    symbol      = item.symbol
    underlying  = item.underlying
    asset       = item.asset
    fees        = shares `sliceOf` (item.fees)
    distance t  = item.distance (t^.item)

instance Render t => Render (Lot t) where
    rendered Lot {..} = tshow _shares
        <> " @@ " <> tshow (_costOfShares^.coerced / _shares)
        $$ nest 2 (rendered _item)

sliceOf :: (Priced t, Functor f)
        => Lens' (Lot t) (Amount 6) -> LensLike' f (Lot t) (Amount n)
        -> LensLike' f (Lot t) (Amount n)
sliceOf _ _ f t | t^.item.quantity == 0 = t <$ f 0
sliceOf n l f t = t & l.percent (t^.n / t^.item.quantity) %%~ f

mkLot :: Priced t => t -> Lot t
mkLot t = Lot
    { _shares       = t^.quantity
    , _costOfShares = t^.cost
    , _item         = t
    , _trail        = []
    }

alignLots :: (Priced a, Priced b) => a -> b -> (Split a, Split b)
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
    | Submit (Event t)
    | ReplaceEvent (Event t) (Event t)
    | ReturnEvent (Event t)
    | InsertEvent (Event t)
    | AddEvent (Event t)
    deriving (Eq, Ord, Show)

makePrisms ''StateChange

instance (Transactional t, Render t) => Render (StateChange t) where
    rendered = \case
        Result x         -> "Result"       $$ nest 2 (rendered x)
        Submit x         -> "Submit"       $$ nest 2 (rendered x)
        ReplaceEvent x y -> "ReplaceEvent" $$ nest 2 (rendered x $$ rendered y)
        ReturnEvent x    -> "ReturnEvent"  $$ nest 2 (rendered x)
        InsertEvent x    -> "InsertEvent"  $$ nest 2 (rendered x)
        AddEvent x       -> "AddEvent"     $$ nest 2 (rendered x)

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
addFees x | Just u <- x^?_OpenPosition._3 =
    (x & _OpenPosition._3.cost -~ u^.fees.coerced)
addFees x = x

foldEvents :: (Transactional t, Render t)
           => Event t
           -> (Event t -> [Event t] -> ChangeState t (Maybe (Event t)))
           -> HistoryState t [StateChange t]
foldEvents t k = do
    evs <- use id
    (ml, sc) <- lift $ flip runStateT [] $
        (\f -> foldlM f (Just t) (tails evs)) $
            \ml es -> case ml of
                Nothing -> Nothing <$ case es of
                    []    -> pure ()
                    (e:_) -> change $ ReturnEvent e
                Just t' -> k t' es
    when (has _Just ml) $
        error $ "foldEvents: unexpected remainder: "
            ++ render (rendered ml)
    pure sc

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
        OptionAssigned Nothing t
    (ReceiveAndDeliver, OptionExpiration) ->
        ClosePosition Nothing t
    (ReceiveAndDeliver, CashAlternativesPurchase) ->
        UnrecognizedTransaction $ t & cost .~ t^.quantity
    (ReceiveAndDeliver, CashAlternativesRedemption) ->
        UnrecognizedTransaction $ t & cost .~ t^.quantity
    (ReceiveAndDeliver, TransferIn) ->
        OpenPosition Long WashSaleIneligible t

    (Trade, BuyTrade)
        | Just API.Open <- t^.effect ->
          OpenPosition Long WashSaleEligible t
        | Just API.Close <- t^.effect ->
          ClosePosition (Just Short) t
        | otherwise ->
          OpenOrClosePosition Short t

    (Trade, SellTrade)
        | Just API.Open <- t^.effect ->
          OpenPosition Short WashSaleEligible t
        | Just API.Close <- t^.effect ->
          ClosePosition (Just Long) t
        | otherwise ->
          OpenOrClosePosition Long t

    (Trade, CloseShortPosition) ->
        ClosePosition (Just Short) t
    (Trade, OptionAssignment) ->
        OptionAssigned Nothing t
    (Trade, BondsRedemption) ->
        UnrecognizedTransaction $ t & cost %~ negate
    (Trade, ShortSale) ->
        OpenPosition Short WashSaleEligible t
    -- (Trade, TradeCorrection) -> pure []

    -- (WireIn, WireIncoming) -> pure []

    _ -> UnrecognizedTransaction t

    -- x -> error $ "Unrecognized transaction type+subtype: " ++ show x

handleEvent :: (Transactional t, Render t)
            => [Event t]        -- ^ historical events
            -> Event t          -- ^ current event
            -> ChangeState t (Maybe (Event t))
              -- ^ recorded change, and what remains
handleEvent (ee@(EstablishEquityCost amt cst dt):_) ev@(OpenPosition disp _ o)
    | not (isOption o || isOptionAssignment o) = do
    let nev = addFees $
            ev & _OpenPosition._2 .~ WashSaleIneligible
               & _OpenPosition._3 .~
                   if amt < o^.quantity
                   then o & quantity .~ amt
                          & cost     .~ cst
                          & time     .~ dt
                   else o & cost     +~ cst^.percent (o^.quantity / amt)
                          & time     .~ dt

    forM_ (nev^?_OpenPosition._1) $ \ed -> unless (disp == ed) $
        throwError $ DispositionMismatch disp ed

    when (amt > quant) $
        change $ ReplaceEvent ee (EstablishEquityCost
                                    (amt - quant)
                                    (cst^.percent ((amt - quant) / amt))
                                    dt)
    change $ AddEvent nev
    change $ Result nev

    pure $ if amt < quant
           then Just $ ev & _OpenPosition._3.quantity -~ amt
           else Nothing
  where
    quant = o^.quantity

handleEvent (ws@(WashSale Deferred adj):_) opos@(OpenPosition _ _ u)
    | adj^?item._PositionClosed._3.symbol == Just (u^.symbol) &&
      adj^?item._PositionClosed._3.distance u.to (<= 30) == Just True = do
    let (s, _) = adj `alignLots` u

    changes $ ReplaceEvent ws . WashSale Deferred <$> s^.._SplitKept
    changes $ Result . WashSale Transferred <$> s^.._SplitUsed

    -- We wash failing closes by adding the amount to the cost basis of
    -- the opening transaction. Thus, we generate three instances of
    -- WashLossApplied, but only one OpenPosition.
    pure $ Just $ opos & _OpenPosition._3.cost +~ s^?!_SplitUsed.cost

{-
-- Opening against an assigned option happens only for a Put.
handleEvent (ev@(OptionAssigned (Just o) c):_) (OpenPosition disp elig u)
    | o^.underlying == u^.symbol && isOptionAssignment u = do
    let (s, d) = (o & quantity *~ 100) `alignLots` u

    changes $ ReplaceEvent ev
        <$> (OptionAssigned <$> s^.._SplitKept.to Just <*> pure c)

    let res = d ^.. _SplitUsed
                  . to (cost +~ s^?!_SplitUsed.cost)
                  . to (assert (not (isCall o)) $ OpenPosition Long elig)
                  . to addFees
        dur | u^.distance o > 365 = Long
            | otherwise           = Short

    changes $ AddEvent <$> res
    changes $ Result
        <$> [ CapitalGain dur (negate (s^?!_SplitUsed.cost)) ev ] ++ res

    pure $ d^?_SplitKept.to (OpenPosition disp elig)

-- Closing against an assigned option happens only for a Call. If it was a
-- covered call, then the case above for matching against an OpenPosition will
-- have handled it.
--
-- NOTE: Unlike the Put case, this code is only used for assigned calls if the
-- assignment does not fully close existing equity positions.
handleEvent (ev@(OptionAssigned (Just o) c):_) (ClosePosition disp u)
    | o^.underlying == u^.symbol && isOptionAssignment u = do
    let (s, d) = o `alignLots` u

    changes $ ReplaceEvent ev
        <$> (OptionAssigned <$> s^.._SplitKept.to Just <*> pure c)

    let res = d ^.. _SplitUsed
                  . to (cost +~ s^?!_SplitUsed.cost)
                  . to (assert (isCall o) $
                          OpenPosition Short WashSaleEligible)
                  . to addFees
        dur | u^.distance o > 365 = Long
            | otherwise           = Short

    changes $ AddEvent <$> res
    changes $ Result <$> [ CapitalGain dur (s^?!_SplitUsed.cost) ev ] ++ res

    pure $ d^?_SplitKept.to (ClosePosition disp)
-}

-- If the option being assigned is a put, then this is either an opening
-- purchase of shares, or closing of a short position.
--
-- If the option being assigned is a call, then this is either a closing of
-- shares purchased earlier, or the opening of a short position.
handleEvent (ev@(OptionAssigned (Just o) c):_) (OptionAssigned Nothing u)
    | o^.underlying == u^.symbol = do
    let (s, d) = (o & quantity *~ 100) `alignLots` u
    changes $ ReplaceEvent ev
        <$> (OptionAssigned <$> s^.._SplitKept.to Just <*> pure c)
    let adj = d^?!_SplitUsed & cost +~ s^?!_SplitUsed.cost
        dur | c^.distance o > 365 = Long
            | otherwise           = Short
    change $ Result $ CapitalGain dur (s^?!_SplitUsed.cost.to negate) ev
    -- If it is a call, the "action" is to open a short position or close an
    -- existing long position; if a put, then open a long position or close an
    -- existing short position.
    change $ Submit $ OpenOrClosePosition disp adj
    pure $ d^?_SplitKept.to (OptionAssigned Nothing)
  where
    disp | isCall o  = Long
         | otherwise = Short

handleEvent (opos@(OpenPosition ed WashSaleEligible u):_) (WashSale Deferred adj)
    | adj^?item._PositionClosed._3.symbol == Just (u^.symbol) &&
      adj^?item._PositionClosed._3.distance u.to (<= 30) == Just True = do
    let (s, d) = u `alignLots` adj
        rep = s^?!_SplitUsed & cost +~ d^?!_SplitUsed.cost
        opos' = opos & _OpenPosition._2 .~ WashSaleIneligible
                     & _OpenPosition._3 .~ rep

    change $ ReplaceEvent opos opos'
    changes $ Result <$>
        (d^.._SplitUsed.to (WashSale Immediate) ++
         s^.._SplitUsed.to (PositionClosed ed (s^?!_SplitUsed)) ++
         [ opos' ])

    -- We wash failing closes by adding the amount to the cost basis of
    -- the opening transaction. Thus, we generate three instances of
    -- WashLossApplied, but only one OpenPosition.
    pure $ WashSale Deferred <$> d^?_SplitKept

handleEvent (OpenPosition ed elig o:_evs) pos
    | Just disp <-
        pos^?failing (_ClosePosition._1.non ed)
                     (failing (_OpenOrClosePosition._1)
                              (\f s -> s <$ f Short)),
      Just u <- pos^?failing (_ClosePosition._2)
                            (failing (_OpenOrClosePosition._2)
                                     (_OptionAssigned._2)),
      ed == disp,
      o^.symbol == u^.symbol = do
    let (s, d) = o `alignLots` u

    forM_ ((,) <$> s^?_SplitUsed <*> d^?_SplitUsed) $ \(su, du) -> do
        let res = PositionClosed ed su du
            adj = s^?!_SplitUsed.cost +
                  d^?!_SplitUsed.cost - d^?!_SplitUsed.fees.coerced
            typ | adj > 0   = CapitalGain
                | otherwise = CapitalLoss
            dur | du^.distance su > 365 = Long
                | otherwise             = Short

        changes $ Result <$> [ typ dur (adj^.coerced) res, res ]
        changes $ ReturnEvent <$>
            s^.._SplitKept.to (OpenPosition ed WashSaleIneligible)

        when (has _OptionAssigned pos) $
            change $ AddEvent $ pos & _OptionAssigned._1 ?~ o

        -- After closing at a loss, and if the loss occurs within 30 days
        -- of its corresponding open, and there is another open within 30
        -- days of the loss, close it and re-open so it's repriced by the
        -- wash loss.
        when (u^.distance o <= 30 && adj < 0 && elig == WashSaleEligible) $
            change $ Submit $ WashSale Deferred $ washSale res

    pure $ d^?_SplitKept <&> \k ->
        pos & _ClosePosition._2       .~ k
            & _OpenOrClosePosition._2 .~ k
            & _OptionAssigned._2      .~ k

handleEvent (e:_) u = do
    change $ ReturnEvent e
    pure $ Just u

handleEvent [] (ClosePosition _ u) = do
    throwError $ CannotFindOpen u

-- This action will close any open position that matches, but if none does, it
-- means we should open a position with the inverse disposition.
handleEvent [] (OpenOrClosePosition disp u) = do
    let r = addFees $ OpenPosition (invertDisposition disp)
                                   WashSaleEligible u
    change $ AddEvent r
    change $ Result r
    pure Nothing

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

handleChanges :: [StateChange t]
              -> HistoryState t ([Event t], [Event t])
handleChanges sc = do
    id .= sc^..each._InsertEvent
       ++ sc^..each.failing (_ReplaceEvent._2) _ReturnEvent
       ++ sc^..each._AddEvent
    pure (sc^..each._Submit, sc^..each._Result)

gainsKeeper :: (Transactional t, Eq t, Render t)
            => Options -> t -> State (GainsKeeperState t) [Event (Lot t)]
gainsKeeper opts t =
    zoom (positionEvents.at (t^.underlying.non "").non []) $ do
        hist <- use id
        let doc = ""
                $$ "EVN" <> space <> rendered event
                $$ "" $$ "bef" <> space <> rendered hist
        eres <- runStateExcept $ do
            chg <- applyEvent event
            (resubmit, res) <- handleChanges chg
            res' <- forM resubmit $ \ev -> do
                chg' <- applyEvent ev
                snd <$> handleChanges chg'
            hist' <- use id
            let fin  = res ++ concat res'
                doc' = "" $$ "chg" <> space <> rendered chg
                    $$ "" $$ "sub" <> space <> rendered resubmit
                    $$ "" $$ "AFT" <> space <> rendered hist'
                    $$ "" $$ "RES" <> space <> rendered fin
                    $$ "" $$ "--------------------------------------------------"
            pure (doc', fin)
        case eres of
            Left err -> do
                error $ render $ doc
                    $$ "" $$ "ERR" <> space <> rendered err
                    $$ "" $$ "--------------------------------------------------"
            Right (doc', res) -> do
                when (matches t) $ renderM $ doc $$ doc'
                pure res
  where
    event = eventFromTransaction (mkLot t & trail .~ [transactionRef t])
    applyEvent = foldEvents ?? (flip handleEvent)

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
            Just i   -> show (x^.ident) `isInfixOf` i

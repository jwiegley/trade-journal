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

module ThinkOrSwim.Event where

-- ( gainsKeeper,
--   GainsKeeperState (..),
--   newGainsKeeperState,
--   positionEvents,
--   roundingEntries,
--   Lot,
--   shares,
--   price,
--   item,
--   trail,
--   Event (..),
--   Action (..),
--   _OpenPosition,
--   _ClosePosition,
--   Disposition (..),
--   Priced (..),
--   TaxBracket (..),
--   Eligibility (..),
--   Transactional (..),
--   isCall,
--   isOption,
--   isOptionAssignment,
--   renderRefList,
-- )

{-
import Control.Applicative
import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.State
import Data.Amount
import Data.Foldable
import Data.List (intersperse, isInfixOf, tails)
import Data.Map (Map)
import Data.Split
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Utils
import Text.PrettyPrint as P
import ThinkOrSwim.API.TransactionHistory.GetTransactions
  ( AssetType (..),
    TransactionSubType (..),
    TransactionType (..),
    _OptionAsset,
  )
import qualified ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import ThinkOrSwim.Options (Options)
import qualified ThinkOrSwim.Options as Options
import Prelude hiding ((<>), Double, Float)

data Disposition
  = Long
  | Short
  deriving (Eq, Ord, Show, Enum, Bounded)

makePrisms ''Disposition

data TaxBracket
  = LongTerm
  | ShortTerm
  | Collectible
  deriving (Eq, Ord, Show, Enum, Bounded)

makePrisms ''TaxBracket

data Eligibility
  = WashSaleEligible
  | WashSaleIneligible
  deriving (Eq, Ord, Show, Enum, Bounded)

makePrisms ''Eligibility

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

-- Events are temporarily recorded in a history, and may have an impact on
-- future transactions or P/L.
data Event t
  = PositionOpened Disposition Eligibility t
  | PositionClosed Disposition t t
  | PendingWashLoss (Lot (Event t))
  | OptionAssigned t t
  | TransactionSplit API.TransactionId [Amount 6]
  | EquityCostBasis
      { _equityAmount :: Amount 6,
        _equityPrice :: Amount 6,
        _equityDate :: UTCTime
      }
  deriving (Eq, Ord, Show)

instance (Transactional t, Render t) => Render (Event t) where
  rendered = \case
    PositionOpened d e x ->
      "PositionOpened"
        <> space
        <> tshow d
        <> space
        <> tshow e
        $$ space <> space <> rendered x
    PositionClosed d o c ->
      "PositionClosed"
        <> space
        <> tshow d
        $$ space <> space <> rendered o
        $$ space <> space <> rendered c
    PendingWashLoss x ->
      "PendingWashLoss"
        $$ space <> space <> rendered x
    OptionAssigned x y ->
      "OptionAssigned"
        $$ space <> space <> rendered x
        $$ space <> space <> rendered y
    TransactionSplit x xs ->
      "TransactionSplit"
        <> space
        <> tshow x
        <> space
        <> renderList tshow xs
    EquityCostBasis x y z ->
      "EquityCostBasis"
        <> space
        <> tshow x
        <> space
        <> tshow y
        <> space
        <> tshow z

data EventError t
  = DispositionMismatch Disposition Disposition
  | CannotFindOpen t
  deriving (Eq, Ord, Show)

instance (Transactional t, Render t) => Render (EventError t) where
  rendered = \case
    DispositionMismatch x y ->
      "DispositionMismatch" <> space <> tshow x <> space <> tshow y
    CannotFindOpen x ->
      "CannotFindOpen"
        $$ space <> space <> rendered x

-- Actions can have an impact on events.
data Action t
  = OpenPosition Disposition Eligibility t
  | ClosePosition Disposition t
  | WashLoss (Lot (Action t))
  deriving (Eq, Ord, Show)

instance (Transactional t, Render t) => Render (Action t) where
  rendered = \case
    OpenPosition d e x ->
      "OpenPosition"
        <> space
        <> tshow d
        <> space
        <> tshow e
        $$ space <> space <> rendered x
    ClosePosition d x ->
      "ClosePosition"
        <> space
        <> tshow d
        $$ space <> space <> rendered x
    CapitalGain b g x ->
      "CapitalGain"
        <> space
        <> tshow b
        <> space
        <> tshow g
        $$ space <> space <> rendered x
    CapitalLoss b g x ->
      "CapitalLoss"
        <> space
        <> tshow b
        <> space
        <> tshow g
        $$ space <> space <> rendered x
    WashLoss x ->
      "WashLoss"
        $$ space <> space <> rendered x
    WashDeferred x ->
      "WashDeferred"
        $$ space <> space <> rendered x
    RoundTransaction rnd ->
      "RoundedTransaction"
        <> space
        <> rendered rnd
    Unrecognized t ->
      "Unrecognized"
        $$ space <> space <> rendered t

-- Results are the fruit of applying actions to the events, or just a result
-- to be immediate reported.
data Adjustment t
  = GainLoss (Event t)
  | Washed (Event t)
  | Rounded
  deriving (Eq, Ord, Show, Enum, Bounded)

makePrisms ''Adjustment

data Adjusted t
  = Adjusted
      { _adjKind :: Adjustment,
        _adjBracket :: TaxBracket,
        _adjAmount :: Amount 6,
        _adjItem :: Lot (Action t)
      }
  deriving (Eq, Ord, Show)

makeLenses ''Adjusted

                $$ space <> space <> rendered x
        CapitalGain b g x ->
            "CapitalGain"
                <> space <> tshow b
                <> space <> tshow g
                $$ space <> space <> rendered x
        CapitalLoss b g x ->
            "CapitalLoss"
                <> space <> tshow b
                <> space <> tshow g
                $$ space <> space <> rendered x
        WashLoss x ->
            "WashLoss"
                $$ space <> space <> rendered x
        WashDeferred x ->
            "WashDeferred"
                $$ space <> space <> rendered x
        RoundTransaction rnd ->
            "RoundedTransaction"
                <> space <> rendered rnd
        Unrecognized t ->
            "Unrecognized"
                $$ space <> space <> rendered t

class Priced t where
    quantity :: Lens' t (Amount 6)
    price    :: Lens' t (Amount 6)

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
    price    | API.xamount == 0 = 0
             | otherwise        = = API.xcost / API.xamount

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
        WashSaleRule wash -> "W$" <> tshow wash <> "/" <> tshow _refId
        RollingOrder roll -> "R$" <> tshow roll <> "/" <> tshow _refId
        OpeningOrder      -> "" <> tshow _refId
        ExistingEquity    -> "Equity"

transactionRef :: Transactional t => t -> Ref
transactionRef t = Ref OpeningOrder (t^.ident)

renderRefList :: [Ref] -> Doc
renderRefList = mconcat . intersperse "," . map rendered

data Lot t = Lot
    { _shares :: Amount 6
    , _price  :: Amount 6
    , _item   :: t
    , _trail  :: [Ref]
    }
    deriving (Eq, Ord, Show)

makePrisms ''RefType
makeLenses ''Lot
makePrisms ''Action
makePrisms ''Event
makePrisms ''EventError

instance Priced (Lot t) where
    quantity = shares
    cost     = costOfShares

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
        <> " @@ " <> tshow (if _shares == 0
                           then 0
                           else _costOfShares^.coerced / _shares)
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

data StateChange t
    = Result       (Action t)
    | Submit       (Action t)
    | ReplaceEvent (Event t) (Event t)
    | ReturnEvent  (Event t)
    | InsertEvent  (Event t)
    | AddEvent     (Event t)
    deriving (Eq, Ord, Show)

makePrisms ''StateChange

instance (Transactional t, Render t) => Render (StateChange t) where
    rendered = \case
        Result       x   -> "Result"       $$ nest 2 (rendered x)
        Submit       x   -> "Submit"       $$ nest 2 (rendered x)
        ReplaceEvent x y -> "ReplaceEvent" $$ nest 2 (rendered x $$ rendered y)
        ReturnEvent  x   -> "ReturnEvent"  $$ nest 2 (rendered x)
        InsertEvent  x   -> "InsertEvent"  $$ nest 2 (rendered x)
        AddEvent     x   -> "AddEvent"     $$ nest 2 (rendered x)

type EventStateT s t a = StateT s (Except (EventError t)) a
type HistoryState  t a = EventStateT [Event t] t a
type ChangeState   t a = EventStateT [StateChange t] t a

isOption :: Transactional t => t -> Bool
isOption = has (asset._OptionAsset)

isOptionAssignment :: Transactional t => t -> Bool
isOptionAssignment t = t^.subkind == OptionAssignment

isCall :: Transactional t => t -> Bool
isCall t = t^?asset._OptionAsset.API.putCall == Just API.Call

{------------------------------------------------------------------------}

-- Fees paid to open are borne in the cost basis of the asset
addFeesToEvent :: Transactional t => Event t -> Event t
addFeesToEvent x@(PositionOpened _ _ t) =
    x & _PositionOpened._3.cost -~ t^.fees.coerced
addFeesToEvent x = x

addFeesToAction :: Transactional t => Action t -> Action t
addFeesToAction x@(OpenPosition _ _ t) =
    x & _OpenPosition._3.cost -~ t^.fees.coerced
addFeesToAction x = x

foldEvents :: (Transactional t, Render t)
           => Action t
           -> (Action t -> [Event t] -> ChangeState t (Maybe (Action t)))
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

matchPositions :: (Render t, Transactional t)
               => Maybe Disposition -> t -> [Event t] -> [Action t]
matchPositions disp t hist =
    let (mu, evs) = foldl' f (Just t, []) hist
    in reverse $ case mu of
        Nothing -> evs
        Just u  -> case disp of
            Nothing -> error $ "Cannot match positions with "
                          ++ render (rendered t)
            Just d  ->
                OpenPosition (invertDisposition d) WashSaleEligible u : evs
  where
    invertDisposition = \case
        Short -> Long
        Long  -> Short

    f (Just u, evs) (PositionOpened disp' _ o)
        | t^.symbol == o^.symbol,
          maybe True (== disp') disp =
          let (_s, d) = o `alignLots` u
          in ( d^?_SplitKept
             , ClosePosition disp' (d^?!_SplitUsed) : evs )
    f (mu, evs) _ = (mu, evs)

actionsFromTransaction :: (Transactional t, Render t, Eq t)
                      => t -> [Event t] -> [Either (Action t) (Result t)]
actionsFromTransaction t evs = case (t^.kind, t^.subkind) of
    -- (DividendOrInterest, AdrFee) -> undefined
    -- (DividendOrInterest, FreeBalanceInterestAdjustment) -> undefined
    -- (DividendOrInterest, OffCycleInterest) -> undefined
    -- (DividendOrInterest, SecuritiesInterestIncome) -> undefined
    -- (DividendOrInterest, ForeignTaxWithheld) -> undefined
    -- (DividendOrInterest, QualifiedDividend) -> undefined

    -- (ElectronicFund, TransferFromCashAccount) -> undefined
    -- (ElectronicFund, DirectDeposit) -> undefined
    -- (ElectronicFund, TransferToFuturesAccount) -> undefined

    -- (Journal, TransferFromFuturesAccount) -> undefined
    -- (Journal, TransferToFuturesAccount) -> undefined
    -- (Journal, MiscellaneousJournalEntry) -> undefined
    -- (Journal, MarkToMarket) -> undefined
    -- (Journal, CashAlternativesPurchase) -> undefined
    -- (Journal, Rebate) -> undefined
    -- (Journal, CashAlternativesRedemption) -> undefined
    -- (Journal, TransferFromForexAccount) -> undefined
    -- (Journal, TransferToForexAccount) -> undefined

    -- (ReceiveAndDeliver, InternalTransfer) -> undefined
    -- Option assignment is much like an expiration, except P/L is carried
    -- over to the subsequent equity transaction: the cost basis if
    -- purchasing, or the gain/loss if selling. This equity transfer is a
    -- (Trade, OptionAssignment) transaction and follows this event.
    (ReceiveAndDeliver, OptionAssignment) ->
        matchPositions (Just Short) t evs
    (ReceiveAndDeliver, OptionExpiration) ->
        matchPositions Nothing t evs
    (ReceiveAndDeliver, CashAlternativesPurchase) ->
        [Unrecognized $ t & cost .~ t^.quantity]
    (ReceiveAndDeliver, CashAlternativesRedemption) ->
        [Unrecognized $ t & cost .~ t^.quantity]
    (ReceiveAndDeliver, TransferIn) ->
        [OpenPosition Long WashSaleIneligible t]

    (Trade, BuyTrade)
        | Just API.Open <- t^.effect ->
          [OpenPosition Long WashSaleEligible t]
        | Just API.Close <- t^.effect ->
          [ClosePosition Short t]
        | t^.cost < 0 ->
          [OpenPosition Long WashSaleEligible t]
        | otherwise ->
          [ClosePosition Short t]

    (Trade, SellTrade)
        | Just API.Open <- t^.effect ->
          [OpenPosition Short WashSaleEligible t]
        | Just API.Close <- t^.effect ->
          [ClosePosition Long t]
        | t^.cost > 0 ->
          [OpenPosition Short WashSaleEligible t]
        | otherwise ->
          [ClosePosition Long t]

    (Trade, CloseShortPosition) ->
        [ClosePosition Short t]
    (Trade, OptionAssignment)
        | t^.cost > 0 ->
            -- If this part of the options assignment is a due to a call, then
            -- we are selling shares, otherwise we are buying shares.
            matchPositions (Just Long) t evs
        | otherwise ->
            matchPositions (Just Short) t evs
    (Trade, BondsRedemption) ->
        [Unrecognized $ t & cost %~ negate]
    (Trade, ShortSale) ->
        [OpenPosition Short WashSaleEligible t]
    -- (Trade, TradeCorrection) -> undefined

    -- (WireIn, WireIncoming) -> undefined

    _ -> [Unrecognized t]

    -- x -> error $ "Unrecognized transaction type+subtype: " ++ show x

handleAction :: (Transactional t, Render t)
             => [Event t]        -- ^ historical events
             -> Action t          -- ^ current event
             -> ChangeState t (Maybe (Action t))
               -- ^ recorded change, and what remains
handleAction (ee@(EquityCost amt cst dt):_) pos@(OpenPosition od _ o)
    | not (isOption o || isOptionAssignment o) = do
    let o' | amt < quant =
             o & quantity .~ amt
               & cost     .~ cst
               & time     .~ dt
           | otherwise =
             o & cost     +~ cst^.percent (quant / amt)
               & time     .~ dt

    when (amt > quant) $
        change $ ReplaceEvent ee $
            EquityCost
                (amt - quant)
                (cst^.percent ((amt - quant) / amt))
                dt

    change $ AddEvent $ addFeesToEvent $
        PositionOpened od WashSaleIneligible o'
    change $ Result $ addFeesToAction $
        OpenPosition od WashSaleIneligible o'

    pure $ if amt < quant
           then Just $ pos & _OpenPosition._3.quantity -~ amt
           else Nothing
  where
    quant = o^.quantity

{-
handleAction (ws@(WashLoss adj):_) (OpenPosition _ _ u)
    | Just orig <- adj^?item._PositionClosed._3,
      orig^.symbol == u^.symbol,
      orig^.distance u.to (<= 30) = do
    let (s, _) = adj `alignLots` u

    changes $ ReplaceEvent ws . WashLoss <$> s^.._SplitKept
    changes $ Result . AdjustCostBasis <$> s^.._SplitUsed

    -- We wash failing closes by adding the amount to the cost basis of
    -- the opening transaction. Thus, we generate three instances of
    -- WashLossApplied, but only one OpenPosition.
    let cst = s^?!_SplitUsed.cost
    pure $ Just $ opos
        & _OpenPosition._3.cost        +~ cst
        & _OpenOrClosePosition._2.cost +~ cst

-- If the option being assigned is a put, then this is either an opening
-- purchase of shares, or closing of a short position.
--
-- If the option being assigned is a call, then this is either a closing of
-- shares purchased earlier, or the opening of a short position.
handleAction (ev@(OptionAssigned (Just o) c):_) (AssignOption u)
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

handleAction (o@(PositionOpened ed WashSaleEligible u):_) (WashLoss Deferred adj)
    | adj^?item._PositionClosed._3.symbol == Just (u^.symbol) &&
      adj^?item._PositionClosed._3.distance u.to (<= 30) == Just True = do
    let (s, d) = u `alignLots` adj
        rep    = s^?!_SplitUsed & cost +~ d^?!_SplitUsed.cost
        o'     = o & _OpenPosition._2 .~ WashSaleIneligible
                   & _OpenPosition._3 .~ rep

    change $ ReplaceEvent o o'
    changes $ Result <$>
        (d^.._SplitUsed.to (WashSale Immediate) ++
         s^.._SplitUsed.to (WashTransaction .
                            PositionClosed ed (s^?!_SplitUsed)) ++
         [ WashTransaction o' ])

    -- We wash failing closes by adding the amount to the cost basis of
    -- the opening transaction. Thus, we generate three instances of
    -- WashLossApplied, but only one OpenPosition.
    pure $ WashSale Deferred <$> d^?_SplitKept
-}

handleAction (PositionOpened od _ o : _) pos@(ClosePosition cd c)
    | od == cd, o^.symbol == c^.symbol = do
    let (s, d) = o `alignLots` c

    forM_ ((,) <$> s^?_SplitUsed <*> d^?_SplitUsed) $ \(su, du) -> do
        let adj = su^.cost + du^.cost - du^.fees.coerced
            typ | adj > 0   = CapitalGain
                | otherwise = CapitalLoss
            dur | du^.distance su > 365 = LongTerm
                | otherwise             = ShortTerm
            ev  = PositionClosed od su du
            res = ClosePosition od du

        changes $ Result <$> [ typ dur (adj^.coerced) ev, res ]
        changes $ ReturnEvent <$>
            s^.._SplitKept.to (PositionOpened od WashSaleIneligible)

        -- jww (2020-05-31): Check
        -- when (has _OptionAssigned pos) $
        --     change $ AddEvent $ pos & _OptionAssigned._1 ?~ o

        -- After closing at a loss, and if the loss occurs within 30 days
        -- of its corresponding open, and there is another open within 30
        -- days of the loss, close it and re-open so it's repriced by the
        -- wash loss.
        when (c^.distance o <= 30 && adj < 0) $
            change $ Submit $ WashLoss Lot
                { _shares       = du^.quantity
                , _costOfShares = adj
                , _item         = res
                , _trail        = []
                }

    pure $ d^?_SplitKept <&> \k -> pos & _ClosePosition._2 .~ k

handleAction (e:_) u = do
    change $ ReturnEvent e
    pure $ Just u

handleAction [] (ClosePosition _ c) = do
    throwError $ CannotFindOpen c

handleAction [] pos@(OpenPosition d e o) = do
    change $ AddEvent $ addFeesToEvent $ PositionOpened d e o
    change $ Result $ addFeesToAction pos
    pure Nothing

handleAction [] t = do
    change $ Result $ addFeesToAction t
    pure Nothing

{------------------------------------------------------------------------}

data GainsKeeperState t = GainsKeeperState
    { _positionEvents  :: Map Text [Event (Lot t)]
    , _roundingEntries :: Map Text (Amount 2)
    }
    deriving (Eq, Ord, Show)

makeLenses ''GainsKeeperState

newGainsKeeperState :: GainsKeeperState t
newGainsKeeperState = GainsKeeperState
    { _positionEvents  = mempty
    , _roundingEntries = mempty
    }

runStateExcept :: StateT s (Except e) a -> State s (Either e a)
runStateExcept (StateT f) = StateT $ \s -> do
    eres <- runExceptT (f s)
    pure $ case eres of
        Left err        -> (Left err, s)
        Right (res, s') -> (Right res, s')

handleChanges :: [StateChange t]
              -> HistoryState t ([Action t], [Action t])
handleChanges sc = do
    id .= sc^..each._InsertEvent
       ++ sc^..each.failing (_ReplaceEvent._2) _ReturnEvent
       ++ sc^..each._AddEvent
    pure (sc^..each._Submit, sc^..each._Result)

gainsKeeper :: (Transactional t, Eq t, Render t)
            => Options -> t -> State (GainsKeeperState t) [Action (Lot t)]
gainsKeeper opts t = do
    mround <- preuse (roundingEntries.ix (T.pack (show (t^.ident))))
    zoom (positionEvents.at (t^.underlying.non "").non []) $ do
        hist <- use id
        let actions = mkActions hist
            doc = ""
                $$ "XCT" <> space <> rendered t
                $$ "" $$ "ACT" <> space <> rendered actions
                $$ "" $$ "bef" <> space <> rendered hist
        eres <- runStateExcept $ do
            (mconcat -> doc', concat -> res) <-
                fmap unzip $ forM actions $ \action -> do
                    chg <- applyAction action
                    (resubmit, res) <- handleChanges chg
                    let doc' = "" $$ "chg" <> space <> rendered chg
                            $$ "" $$ "sub" <> space <> rendered resubmit
                    res' <- forM resubmit $
                        fmap snd . handleChanges <=< applyAction
                    pure (doc', res ++ concat res')
            hist' <- use id
            let doc'' = doc $$ doc'
                     $$ "" $$ "AFT" <> space <> rendered hist'
                     $$ "" $$ "RES" <> space <> rendered res
                     $$ "" $$ "--------------------------------------------------"
            pure (doc'', res)
        case eres of
            Left err ->
                error $ render $ doc
                    $$ "" $$ "ERR" <> space <> rendered err
                    $$ "" $$ "--------------------------------------------------"
            Right (doc', res) -> do
                when (matches t) $ renderM $ doc'
                pure $ res ++ mround^..traverse.to RoundTransaction
  where
    mkActions = actionsFromTransaction (mkLot t & trail .~ [transactionRef t])
    applyAction = foldEvents ?? (flip handleAction)

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
-}

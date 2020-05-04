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
import           Prelude hiding (Float, Double, (<>))
import           Text.PrettyPrint as P
import           ThinkOrSwim.API.TransactionHistory.GetTransactions
                     (TransactionSubType(..), AssetType(..), _OptionAsset)
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

class Show t => Transactional t where
    ident       :: Getter t API.TransactionId
    time        :: Lens' t UTCTime
    kind        :: Getter t API.TransactionSubType
    instruction :: Getter t (Maybe API.Instruction)
    cusip       :: Getter t Text
    symbol      :: Getter t Text
    underlying  :: Getter t Text
    asset       :: Getter t AssetType
    quantity    :: Lens' t (Amount 6)
    cost        :: Lens' t (Amount 6)
    fees        :: Getter t (Amount 2)
    distance    :: t -> Lens' t Integer

instance Transactional API.Transaction where
    ident          = API.xid
    time           = API.xdate
    kind           = API.xsubType
    instruction    = API.xitem.API.instruction
    cusip f s      = s <$ f (s^?!API.xcusip)
    symbol         = API.xsymbol
    underlying f s = s <$ f (s^?!API.xunderlying)
    asset f s      = s <$ f (s^?!API.xasset)
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
        rendered (l^.symbol)
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
isOptionAssignment t = t^.kind == OptionAssignment

intoEvent :: Transactional t => Lot t -> EventState t (Event (Lot t))
intoEvent t = case t^.kind of
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

foldEvents :: Transactional t
           => Lot t
           -> ([Event (Lot t)] -> [Event (Lot t)])
           -> (Lot t -> Event (Lot t) -> EventState t (StateChange (Lot t)))
           -> EventState t [Event (Lot t)]
foldEvents t g k = do
    events <- use id
    sc' <- (\f -> foldlM f (newStateChange t) events) $
        \sc e -> case sc^.leftover of
            Nothing -> pure $ sc & replacements <>~ [e]
            Just t' -> do
                sc' <- k t' e
                pure $ sc'
                     & results      %~ (sc^.results ++)
                     & replacements %~ (sc^.replacements ++)
                     & newEvents    %~ (sc^.newEvents ++)
    left <- traverse intoEvent (sc'^.leftover)
    let res = g (maybeToList left)
    id .= sc'^.replacements ++ sc'^.newEvents ++ res
    pure $ res ++ sc'^.results

washSaleEligible :: Transactional t => t -> Bool
washSaleEligible t = t^.kind /= TransferIn

gain :: Transactional t => Getter (Event t) (Amount 2)
gain f s@(ClosePosition _ o c) =
    s <$ f (coerce (o^.cost + c^.cost) - c^.fees)
gain f s = s <$ f 0

costs :: Transactional t => Getter (Event t) (Amount 6)
costs f = \case
    s@(OpenPosition _ t)            -> s <$ f (t^.cost)
    s@(ClosePosition _ _ t)         -> s <$ f (t^.cost)
    s@(AdjustCostBasisForOpen _ ev) -> s <$ f (ev^.costs)
    s@(AdjustCostBasis _ _ ev)      -> s <$ f (ev^.costs)
    s@(RememberWashSaleLoss _ ev)   -> s <$ f (ev^.costs)
    s@(OptionAssigned _ t)          -> s <$ f (t^.cost)
    s@(ExerciseOption _ t)          -> s <$ f (t^.cost)
    s@(EstablishEquityCost _ c _)   -> s <$ f c

openPosition :: Transactional t
             => Disposition -> Lot t -> EventState t [Event (Lot t)]
openPosition disp t = foldEvents t addFees $ \u e -> case e of
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
            { _leftover     = Just $ u
                & cost +~ o^.cost.percent part.coerced
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
  where
    addFees [] = []
    addFees (x:xs) =
        -- Fees paid to open are borne in the cost basis of the asset
        (x & _OpenPosition._2.cost -~ t^.fees.coerced.percent part) : xs
      where
        -- Thanks to laziness, only evaluated when it works
        part = x^?!_OpenPosition._2.quantity / t^.quantity

closePosition :: Transactional t
              => (Event (Lot t) -> Bool) -> Lot t
              -> EventState t [Event (Lot t)]
closePosition p t = foldEvents t id $ \u e -> case e of
    ev@(OpenPosition ed o) | p ev && o^.symbol == u^.symbol -> do
        let (s, d) = o `alignLots` u
            res = maybeToList $
                (if u^.kind == OptionAssignment
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

processTransaction :: (Transactional t, Eq t)
                   => t -> EventState t [Event (Lot t)]
processTransaction = go . mkLot
  where
    go t = case t^.kind of
        BuyTrade           -> openPosition Long t
        ShortSale          -> openPosition Short t
        TransferIn         -> openPosition Long t
        SellTrade          -> closePosition (openIs Long) t
        CloseShortPosition -> closePosition (openIs Short) t
        OptionExpiration   -> closePosition (const True) t

        -- Assignment is much like an expiration, except P/L is carried over
        -- to the equity transfer: cost basis if purchasing, or reported as
        -- gain/loss if selling. Equity transfer is also an OptionAssignment,
        -- and follows this event.
        OptionAssignment
            | isOption t ->
                closePosition (openIs Short) t
            | t^.instruction == Just API.Buy ->
                openPosition Long t
            | otherwise ->
                closePosition (openIs Long) t

        _ -> pure []

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
gainsKeeper opts t = zoom (positionEvents.at (t^.underlying).non []) $ do
    hist <- use id
    eres <- runStateExcept (processTransaction t)
    let doc = ""
            $$ "xct -> " <> rendered (t^.symbol)
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
        Options.traceAll opts
      || case Options.traceSymbol opts of
            Nothing  -> False
            Just sym -> x^.symbol == T.pack sym
      || case Options.traceUnderlying opts of
            Nothing  -> False
            Just sym -> x^.underlying == T.pack sym
      || case Options.traceId opts of
            Nothing  -> False
            Just i   -> show (x^.ident) == i

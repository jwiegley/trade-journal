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
import           Control.Monad.State
import           Data.Amount
import           Data.Coerce
import           Data.Foldable
import           Data.Map (Map)
import           Data.Maybe (maybeToList)
import           Data.Split
import           Data.Text (Text, unpack)
import           Data.Time
import           Data.Utils
import           Prelude hiding (Float, Double, (<>))
import           Text.PrettyPrint
import           Text.Show.Pretty
import           ThinkOrSwim.API.TransactionHistory.GetTransactions (TransactionSubType(..))
import qualified ThinkOrSwim.API.TransactionHistory.GetTransactions as API

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
    | ExpireOption t
    | AssignOption t t
    | ExerciseOption t t
      -- Transfer
    | EstablishEquityCost
        { _equityAmount :: Amount 6
        , _equityCost   :: Amount 6
        , _equityDate   :: UTCTime
        }
    deriving (Eq, Ord, Show)

makePrisms ''Event

class Show t => Transactional t where
    ident      :: Getter t API.TransactionId
    time       :: Lens' t UTCTime
    kind       :: Getter t API.TransactionSubType
    cusip      :: Getter t Text
    symbol     :: Getter t Text
    underlying :: Getter t Text
    quantity   :: Lens' t (Amount 6)
    cost       :: Lens' t (Amount 6)
    fees       :: Getter t (Amount 2)

instance Transactional API.Transaction where
    ident          = API.xid
    time           = API.xdate
    kind           = API.xsubType
    cusip f s      = s <$ f (s^?!API.xcusip)
    symbol         = API.xsymbol
    underlying f s = s <$ f (s^?!API.xunderlying)
    quantity       = API.xamount
    cost           = API.xcost
    fees f s       = s <$ f (sumOf (API.xfees) s)

data Lot t = Lot
    { _shares       :: Amount 6
    , _costOfShares :: Amount 6
    , _xact         :: t
    , _trail        :: [Event (Lot t)]
    }
    deriving (Eq, Ord)

makeLenses ''Lot

instance Transactional t => Transactional (Lot t) where
    ident      = xact.ident
    time       = xact.time
    kind       = xact.kind
    cusip      = xact.cusip
    symbol     = xact.symbol
    underlying = xact.underlying
    quantity   = shares
    fees       = shares `sliceOf` (xact.fees)
    cost       = costOfShares

instance (Transactional t, Show t) => Show (Lot t) where
    show l@(Lot {..}) =
        show _xact
            ++ " " ++ show _shares
            ++ " @@ " ++ show (l^.cost.coerced / _shares)

sliceOf :: (Transactional t, Functor f)
        => Lens' (Lot t) (Amount 6) -> LensLike' f (Lot t) (Amount n)
        -> LensLike' f (Lot t) (Amount n)
sliceOf n l f t =
    t & l.percent (coerce (t^.n / t^.xact.quantity)) %%~ f

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

intoEvent :: Transactional t => Lot t -> Event (Lot t)
intoEvent t = case t^.kind of
    BuyTrade -> OpenPosition Long t
    SellTrade -> error $ "Attempt to open SellTrade: " ++ show t
    ShortSale -> OpenPosition Short t
    CloseShortPosition ->
        error $ "Attempt to open CloseShortPosition: " ++ show t
    TransferOfSecurityOrOptionIn -> OpenPosition Long t
    _ -> error $ "Cannot render directly into event: " ++ show t

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

foldEvents :: Transactional t
           => Lot t
           -> ([Event (Lot t)] -> [Event (Lot t)])
           -> (Lot t -> Event (Lot t) -> StateChange (Lot t))
           -> State [Event (Lot t)] [Event (Lot t)]
foldEvents t g k = do
    events <- use id
    let sc' = (\f -> foldl' f (newStateChange t) events) $
            \sc e -> case sc^.leftover of
                Nothing -> sc & replacements <>~ [e]
                Just t' ->
                    k t' e & results      %~ (sc^.results ++)
                           & replacements %~ (sc^.replacements ++)
                           & newEvents    %~ (sc^.newEvents ++)
        res = g (maybeToList (intoEvent <$> sc'^.leftover))
    id .= sc'^.replacements ++ sc'^.newEvents ++ res
    pure $ res ++ sc'^.results

washSaleEligible :: Transactional t => t -> Bool
washSaleEligible t = t^.kind /= TransferOfSecurityOrOptionIn

loss :: Transactional t => Getter (Event t) (Amount 2)
loss f s@(ClosePosition _ o c) =
    s <$ f (coerce (o^.cost + c^.cost) - c^.fees)
loss f s = s <$ f 0

openPosition :: Transactional t
             => Lot t -> State [Event (Lot t)] [Event (Lot t)]
openPosition t = foldEvents t addFees $ \t' e -> case e of
    EstablishEquityCost amt cst dt ->
        if amt < t'^.quantity
        then let res = [ intoEvent (t' & quantity .~ amt
                                       & cost     .~ cst
                                       & time     .~ dt) ] in
            StateChange
              { _leftover     = Just (t' & quantity -~ amt)
              , _results      = res
              , _replacements = []
              , _newEvents    = res
              }
        else let res = [ intoEvent
                           (t' & cost +~ cst^.percent (t'^.quantity / amt)
                               & time .~ dt) ] in
            StateChange
              { _leftover     = Nothing
              , _results      = res
              , _replacements =
                  [ EstablishEquityCost
                      (amt - t'^.quantity)
                      (cst^.percent ((amt - t'^.quantity) / amt))
                      dt
                  | amt > t'^.quantity ]
              , _newEvents    = res
              }
    _ -> StateChange
          { _leftover     = Just t'
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
              => Disposition -> Lot t -> State [Event (Lot t)] [Event (Lot t)]
closePosition disp t = foldEvents t id $ \t' e -> case e of
    OpenPosition ls o | disp == ls ->
        let (s, d) = o `alignLots` t'
            res = maybeToList (ClosePosition disp
                                 <$> s^?_SplitUsed
                                 <*> d^?_SplitUsed)
        in StateChange
             { _leftover     = d^?_SplitKept
             , _results      = res
             , _replacements = s^.._SplitKept.to (OpenPosition ls)
             , _newEvents    = res
             }
    _ -> StateChange
          { _leftover     = Just t'
          , _results      = []
          , _replacements = [e]
          , _newEvents    = []
          }

processTransaction :: Transactional t
                   => t -> State [Event (Lot t)] [Event (Lot t)]
processTransaction = go . mkLot
  where
    go t = case t^.kind of
        BuyTrade  -> openPosition t
        ShortSale -> openPosition t

        TransferOfSecurityOrOptionIn -> openPosition t

        SellTrade          -> closePosition Long t
        CloseShortPosition -> closePosition Short t

        OptionAssignment ->
            -- If this is the contract, it closes it much like
            -- OptionExpiration, except P/L is carried to the equity transfer,
            -- whose kind is also OptionAssignment, and which will follow this
            -- event.
            pure []

        OptionExpiration -> pure []

        _ -> pure []

data GainsKeeperState t = GainsKeeperState
    { _positionEvents :: Map Text [Event (Lot t)]
    , _nextEventId    :: API.TransactionId
    }
    deriving (Eq, Ord, Show)

makeLenses ''GainsKeeperState

newGainsKeeperState :: GainsKeeperState t
newGainsKeeperState = GainsKeeperState
    { _positionEvents = mempty
    , _nextEventId    = 1
    }

gainsKeeper :: (Transactional t, Eq t)
            => t -> State (GainsKeeperState t) [Event (Lot t)]
gainsKeeper t = zoom (positionEvents.at (t^.symbol).non []) $ do
    hist <- use id
    renderM $ text "before "
        <> text (unpack (t^.symbol))
        <> ": " <> renderList (text . ppShow) hist
    renderM $ text "transaction: " <> text (ppShow t)
    res <- processTransaction t
    renderM $ text "result: " <> renderList (text . ppShow) res
    hist' <- use id
    renderM $ text "after "
        <> text (unpack (t^.symbol))
        <> ": " <> renderList (text . ppShow) hist'
    pure res

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

module ThinkOrSwim.Model where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Data.Amount
import           Data.Coerce
import           Data.Foldable
import           Data.Maybe (fromMaybe, maybeToList)
import           Data.Split
import           Data.Text (Text, unpack)
import           Data.Time
import           Data.Time.Format.ISO8601
import           Data.Utils
import           Prelude hiding (Float, Double, (<>))
import           ThinkOrSwim.API.TransactionHistory.GetTransactions (TransactionSubType(..))
import qualified ThinkOrSwim.API.TransactionHistory.GetTransactions as API

data Event t
    = OpenPosition t
    | ClosePosition t t
    | AdjustCostBasisForOpen t
    | AdjustCostBasis t t
    | RememberWashSaleLoss t
    | TransferEquitiesIn t
    | AssignOption t t
    | ExerciseOption t t
    | ExpireOption t
    deriving (Eq, Ord, Show)

class Show t => Transactional t where
    ident      :: Getter t API.TransactionId
    time       :: Getter t UTCTime
    kind       :: Getter t API.TransactionSubType
    cusip      :: Getter t Text
    symbol     :: Getter t Text
    underlying :: Getter t Text
    quantity   :: Lens' t (Amount 4)
    cost       :: Lens' t (Amount 4)
    fees       :: Getter t (Amount 2)

data Lot t = Lot
    { _shares       :: Amount 4
    , _costOfShares :: Amount 4  -- the cost of a lot may be adjusted
    , _xact         :: t
    , _trail        :: [Event (Lot t)]
    }
    deriving Show

makeLenses ''Lot

sliceOf :: (Transactional t, Functor f)
        => Lens' t (Amount 4) -> LensLike' f t (Amount n)
        -> LensLike' f t (Amount n)
sliceOf n l f t = t & l.percent (coerce (t^.n / t^.quantity)) %%~ f

sliceOf' :: (Transactional t, Functor f)
         => Amount 4 -> LensLike' f t (Amount n)
         -> LensLike' f t (Amount n)
sliceOf' n l f t = t & l.percent (coerce (n / t^.quantity)) %%~ f

instance Transactional t => Transactional (Lot t) where
    ident      = xact.ident
    time       = xact.time
    kind       = xact.kind
    cusip      = xact.cusip
    symbol     = xact.symbol
    underlying = xact.underlying
    quantity   = shares
    cost       = costOfShares
    fees       = shares `sliceOf` (xact.fees)

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
    diff = xq - yq

loss :: Transactional t => Fold (Event t) (Amount 2)
loss f s = case s of
    OpenPosition _           -> pure s
    ClosePosition o c        -> s <$ f (coerce (c^.cost - o^.cost) - sumOf fees c)
    AdjustCostBasisForOpen _ -> pure s
    AdjustCostBasis _ _      -> pure s
    RememberWashSaleLoss _   -> pure s
    TransferEquitiesIn _     -> pure s
    AssignOption _ _         -> pure s
    ExerciseOption _ _       -> pure s
    ExpireOption _           -> pure s

gainsKeeper :: Transactional t => t -> State [Event (Lot t)] [Event (Lot t)]
gainsKeeper = go . mkLot
  where
    go t = case t^.kind of
        BuyTrade -> do
            -- jww (2020-04-30): Need to check for pending wash sales.
            id <>= [OpenPosition t]
            pure [OpenPosition t]
        SellTrade -> do
            events <- use id
            let (mt', reverse -> res, reverse -> evs) =
                    (\f -> foldl' f (Just t, [], []) events) $
                        \acc@(mt', res, evs) e -> case (e, mt') of
                            (OpenPosition o, Just t') ->
                                let (s, d) = o `alignLots` t'
                                    cp = maybeToList (ClosePosition
                                                        <$> s^?_SplitUsed
                                                        <*> d^?_SplitUsed)
                                in ( d^?_SplitKept
                                   , cp ++ res
                                   , cp ++ evs
                                   )
                            _ -> acc
            id .= evs
            pure $ case mt' of
                Just t' ->
                    error $ "Unexpected: short sale from SellTrade: " ++ show t'
                    -- OpenPosition (t' & quantity %~ negate) : res
                Nothing -> res
        _ -> undefined

data Mock = Mock
    { _mockIdent      :: API.TransactionId
    , _mockTime       :: UTCTime
    , _mockKind       :: API.TransactionSubType
    , _mockCusip      :: Text
    , _mockSymbol     :: Text
    , _mockUnderlying :: Text
    , _mockQuantity   :: Amount 4
    , _mockCost       :: Amount 4
    , _mockFees       :: Amount 2
    }

instance Show Mock where
    show Mock {..} =
        show _mockQuantity
            ++ " @@ " ++ show (_mockCost / _mockQuantity)
            ++ " ## \"" ++ iso8601Show (utctDay _mockTime) ++ "\""

makeLenses ''Mock

newMock :: Mock
newMock = Mock
    { _mockIdent      = 0
    , _mockTime       = UTCTime (ModifiedJulianDay 0) 0
    , _mockKind       = BuyTrade
    , _mockCusip      = ""
    , _mockSymbol     = ""
    , _mockUnderlying = ""
    , _mockQuantity   = 0
    , _mockCost       = 0
    , _mockFees       = 0
    }

instance Transactional Mock where
    ident      = mockIdent
    time       = mockTime
    kind       = mockKind
    cusip      = mockCusip
    symbol     = mockSymbol
    underlying = mockUnderlying
    quantity   = mockQuantity
    cost       = mockCost
    fees       = mockFees

(@@) :: Amount 4 -> Amount 4 -> Mock
q @@ c = newMock & mockQuantity .~ q & mockCost .~ q * c

(@@@) :: Amount 4 -> Amount 4 -> Mock
q @@@ c = newMock & quantity .~ q & cost .~ c

(##) :: Mock -> Text -> Mock
p ## d = p & mockTime
    .~ UTCTime day 0
  where
    day = fromMaybe (error $ "Failed to parse: " ++ unpack d)
                    (iso8601ParseM (unpack d))

test_gainsKeeper :: IO ()
test_gainsKeeper = do
    let res = (`runState` []) $ do
            gainsKeeper (10 @@ 20.00 ## "2020-03-21")
            gainsKeeper (10 @@ 30.00 ## "2020-03-22" & mockKind .~ SellTrade)
    print res
    print $ res^?_1._head.loss

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

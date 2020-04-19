{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module ThinkOrSwim.Types where

import           Control.Applicative ((<|>))
import           Control.Lens
import           Data.Amount
import           Data.Foldable (foldl')
import           Data.Ledger as Ledger
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Time
import           Data.Time.Format.ISO8601
import           Prelude hiding (Float, Double, (<>))
import           Text.PrettyPrint
import           ThinkOrSwim.API.TransactionHistory.GetTransactions
                     as API hiding ((<+>))

import qualified Debug.Trace as Trace
traceM :: Applicative f => String -> f ()
-- traceM _ = pure ()
traceM = Trace.traceM

transactionRef :: API.Transaction -> Ref API.Transaction
transactionRef t = Ref OpeningOrder (t^.xactId) (Just t)

data LotApplied a = LotApplied
    { _loss    :: Amount 2
    , _wasOpen :: LotSplit a
    , _close   :: LotSplit a
    }
    deriving (Eq, Ord, Show)

makeClassy ''LotApplied

nothingApplied :: a -> a -> LotApplied a
nothingApplied x y = LotApplied 0 (None x) (None y)

data CalculatedPL = CalculatedPL
    { _losses  :: [LotAndPL API.Transaction]
    , _history :: [CommodityLot API.Transaction]
    , _opening :: [CommodityLot API.Transaction]
    }
    deriving (Eq, Ord, Show)

makeClassy ''CalculatedPL

data TransactionEvent t = TransactionEvent
    { _gainOrLoss :: Maybe (Amount 2)
    , _eventDate  :: Day
    , _eventLot   :: CommodityLot t
    }
    deriving (Eq, Ord)

makeClassy ''TransactionEvent

instance Show (TransactionEvent t) where
    show TransactionEvent {..} =
        show _gainOrLoss
            ++ " // " ++ iso8601Show _eventDate
            ++ " // " ++ showCommodityLot _eventLot

eventToPL :: TransactionEvent t -> LotAndPL t
eventToPL ev = LotAndPL
    { _plKind = case ev^.gainOrLoss of
                    Nothing -> BreakEven
                    -- jww (2020-04-17): Need to factor in time here to
                    -- determine what the gain/loss period should be.
                    Just x | x < 0     -> GainShort
                           | x > 0     -> LossShort
                           | otherwise -> BreakEven
    , _plLoss = fromMaybe 0 (ev^.gainOrLoss)
    , _plLot  = ev^.eventLot
    }

isDateOrdered :: [TransactionEvent t] -> Bool
isDateOrdered [] = True
isDateOrdered [_] = True
isDateOrdered (x:y:xs)
    | x^.eventDate <= y^.eventDate = isDateOrdered (y:xs)
    | otherwise = False

lotDate :: CommodityLot API.Transaction -> Maybe Day
lotDate l = l^.purchaseDate
    <|> utctDay <$> l^?refs._head.refOrig._Just.xactDate

daysApart :: CommodityLot API.Transaction -> Day -> Maybe Integer
daysApart x yd = do
    xd <- lotDate x
    pure $ xd `diffDays` yd

eventFromPL :: Maybe Day -> LotAndPL API.Transaction
            -> TransactionEvent API.Transaction
eventFromPL md LotAndPL {..} = TransactionEvent
    { _gainOrLoss = if _plLoss /= 0 then Just _plLoss else Nothing
    , _eventDate  =
          fromMaybe (error $ "Missing date: " ++ show _plLot)
                    (md <|> lotDate _plLot)
    , _eventLot   = _plLot
    }

renderList :: (a -> Doc) -> [a] -> Doc
renderList _ [] = brackets empty
renderList f ts =
    fst (foldl' go (empty, True) ts) <> space <> rbrack
  where
    go (_, True) x    = (lbrack <> space <> f x, False)
    go (acc, False) x = (acc $$ comma <> space <> f x, False)

data GainsKeeperState t = GainsKeeperState
    { _openTransactions :: Map Text [CommodityLot t]
    , _positionEvents   :: Map Text [TransactionEvent t]
    }
    deriving (Eq, Ord, Show)

makeClassy ''GainsKeeperState

newGainsKeeperState :: GainsKeeperState t
newGainsKeeperState = GainsKeeperState
    { _openTransactions = M.empty
    , _positionEvents   = M.empty
    }

isTransactionSubType :: TransactionSubType -> CommodityLot API.Transaction -> Bool
isTransactionSubType subty y = case y^.refs of
    [r] | Just subty' <- r^?refOrig._Just.transactionInfo_.transactionSubType,
          subty' == subty -> True
    _ -> False

-- Return True if 'x' is an open and 'y' is a close.
pairedCommodityLots :: CommodityLot API.Transaction
                    -> CommodityLot API.Transaction
                    -> Bool
pairedCommodityLots x y
    | Just x' <- x^?asset, Just y' <- y^?asset, x' /= y' = False
    | x^.quantity < 0 && y^.quantity > 0 = True
    | x^.quantity > 0 &&
      (y^.quantity < 0 || isTransactionSubType OptionExpiration y) = True
    | otherwise = False
  where
    asset = refs._head.refOrig._Just.API.instrument_._Just.assetType

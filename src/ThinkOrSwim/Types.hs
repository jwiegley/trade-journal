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
import           Data.Fixed (Pico)
import           Data.Foldable (foldl')
import           Data.Ledger as Ledger
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, unpack)
import           Data.Time
import           Prelude hiding (Float, Double, (<>))
import           Text.PrettyPrint
import           ThinkOrSwim.API.TransactionHistory.GetTransactions
                     as API hiding ((<+>))

import qualified Debug.Trace as Trace
traceM :: Applicative f => String -> f ()
-- traceM _ = pure ()
traceM = Trace.traceM

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
    , _eventDate  :: UTCTime
    , _eventLot   :: CommodityLot t
    }
    deriving (Eq, Ord)

makeClassy ''TransactionEvent

instance Show (TransactionEvent t) where
    show TransactionEvent {..} =
        show _gainOrLoss
            ++ " // " ++ unpack (toIso8601 _eventDate)
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

lotDate :: CommodityLot API.Transaction -> Maybe UTCTime
lotDate l = l^.purchaseDate <|> l^?refs._head.refOrig._Just.xactDate

daysApart :: CommodityLot API.Transaction -> UTCTime -> Maybe Pico
daysApart x yd = do
    xd <- lotDate x
    pure $ nominalDiffTimeToSeconds (xd `diffUTCTime` yd) / 86400

eventFromPL :: Maybe UTCTime -> LotAndPL API.Transaction
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

renderHistoryBeforeAndAfter
    :: String
    -> CommodityLot t
    -> [CommodityLot t]
    -> [CommodityLot t]
    -> [LotAndPL t]
    -> [CommodityLot t]
    -> Doc
renderHistoryBeforeAndAfter sym l hist next res final =
    text sym <> text ": " <> text (showCommodityLot l)
        $$ text " ++> " <> renderList (text . showCommodityLot) hist
        $$ text " --> " <> renderList (text . showCommodityLot) next
        $$ text " ==> " <> renderList (text . showCommodityLot) final
        $$ text " <-- " <> renderList (text . show) res

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
    | Just x' <- x^?refs._head.refOrig._Just.API.instrument_._Just.assetType,
      Just y' <- y^?refs._head.refOrig._Just.API.instrument_._Just.assetType,
      x' /= y' = Trace.trace (show x' ++ " /= " ++ show y') False

pairedCommodityLots x y
    = (x^.quantity < 0 && y^.quantity > 0)
    || (  x^.quantity > 0
       && (  y^.quantity < 0
          || isTransactionSubType OptionExpiration y
         )
      )

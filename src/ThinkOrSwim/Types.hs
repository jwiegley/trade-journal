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
import           Data.Text (Text)
import           Data.Time
import           Prelude hiding (Float, Double, (<>))
import           Text.PrettyPrint
import           ThinkOrSwim.API.TransactionHistory.GetTransactions
                     as API hiding ((<+>))

-- import Debug.Trace
traceM :: Applicative f => String -> f ()
traceM _ = pure ()

data LotApplied t = LotApplied
    { _gain    :: Amount 2
    , _wasOpen :: LotSplit (CommodityLot t)
    , _close   :: LotSplit (CommodityLot t)
    }
    deriving (Eq, Ord, Show)

makeClassy ''LotApplied

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
    deriving (Eq, Ord, Show)

makeClassy ''TransactionEvent

eventToPL :: TransactionEvent t -> LotAndPL t
eventToPL ev = LotAndPL
    { _loss = fromMaybe 0 (ev^.gainOrLoss)
    , _lot  = ev^.eventLot
    }

isDateOrdered :: [TransactionEvent t] -> Bool
isDateOrdered [] = True
isDateOrdered [_] = True
isDateOrdered (x:y:xs)
    | x^.eventDate <= y^.eventDate = isDateOrdered (y:xs)
    | otherwise = False

lotDate :: CommodityLot API.Transaction -> Maybe UTCTime
lotDate l = l^.purchaseDate <|> l^?refs._head.refOrig.xactDate

daysApart :: CommodityLot API.Transaction -> UTCTime -> Maybe Pico
daysApart x yd = do
    xd <- lotDate x
    pure $ nominalDiffTimeToSeconds (xd `diffUTCTime` yd) / 86400

eventFromPL :: Maybe UTCTime -> LotAndPL API.Transaction
            -> TransactionEvent API.Transaction
eventFromPL md LotAndPL {..} = TransactionEvent
    { _gainOrLoss = if _loss /= 0 then Just _loss else Nothing
    , _eventDate  =
      fromMaybe (error $ "Missing date: " ++ show _lot) (md <|> lotDate _lot)
    , _eventLot   = _lot
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
    [r] | r^.refOrig.transactionInfo_.transactionSubType == subty -> True
    _ -> False

-- Return True if 'x' is an open and 'y' is a close.
pairedCommodityLots :: CommodityLot API.Transaction
                    -> CommodityLot API.Transaction
                    -> Bool
pairedCommodityLots x y
    = x^.quantity < 0 && y^.quantity > 0
    || (  x^.quantity > 0
       && (  y^.quantity < 0
          || isTransactionSubType OptionExpiration y
         )
      )

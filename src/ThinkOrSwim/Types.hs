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

import           Control.Lens
import           Data.Amount
import           Data.Foldable (foldl')
import           Data.Ledger as Ledger
import           Data.Map (Map)
import qualified Data.Map as M
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
    { _gain :: Amount 2
    , _used :: Maybe (CommodityLot t) -- the portion subtracted out
    , _kept :: Maybe (CommodityLot t) -- what remains
    , _left :: Maybe (CommodityLot t) -- the portion unsubtracted
    }
    deriving (Eq, Ord, Show)

makeClassy ''LotApplied

data LotAndPL t = LotAndPL
    { _loss :: Amount 2           -- positive is loss, else gain
    , _lot  :: CommodityLot t
    }
    deriving (Eq, Ord)

makeClassy ''LotAndPL

instance Show (LotAndPL t) where
    show x = showCommodityLot (x^.lot) ++ " $ "  ++ show (- x^.loss)

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
    , _eventXact  :: CommodityLot t
    }
    deriving (Eq, Ord, Show)

makeClassy ''TransactionEvent

isDateOrdered :: [TransactionEvent t] -> Bool
isDateOrdered [] = True
isDateOrdered [_] = True
isDateOrdered (x:y:xs)
    | x^.eventDate <= y^.eventDate = isDateOrdered (y:xs)
    | otherwise = False

data EventHistory t = EventHistory
    { _openTransactions :: [CommodityLot t]
    , _positionEvents   :: [TransactionEvent t]
    }
    deriving (Eq, Ord, Show)

makeClassy ''EventHistory

renderList :: (a -> Doc) -> [a] -> Doc
renderList _ [] = brackets empty
renderList f ts =
    fst (foldl' go (empty, True) ts) <> space <> rbrack
  where
    go (_, True) x    = (lbrack <> space <> f x, False)
    go (acc, False) x = (acc $$ comma <> space <> f x, False)

renderEventHistory :: EventHistory t -> Doc
renderEventHistory (EventHistory ot _) =
    renderList (text . showCommodityLot) ot

newEventHistory :: [CommodityLot t] -> EventHistory t
newEventHistory ot = EventHistory
    { _openTransactions = ot
    , _positionEvents   = []
    }

renderHistoryBeforeAndAfter
    :: String
    -> CommodityLot t
    -> EventHistory t
    -> [LotAndPL t]
    -> Maybe (EventHistory t)
    -> Doc
renderHistoryBeforeAndAfter sym l hist res next =
    text sym <> text ": " <> text (showCommodityLot l)
        $$ text " --> " <> renderEventHistory hist
        $$ text " ==> " <> maybe (brackets empty) renderEventHistory next
        $$ text " <-- " <> renderList (text . show) res

type GainsKeeperState t = Map Text (EventHistory t)

newGainsKeeperState :: GainsKeeperState t
newGainsKeeperState = M.empty

isTransactionSubType :: TransactionSubType -> CommodityLot API.Transaction -> Bool
isTransactionSubType subty y = case y^.refs of
    [r] | r^.refOrig.transactionInfo_.transactionSubType == subty -> True
    _ -> False

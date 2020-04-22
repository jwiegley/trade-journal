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
{-# LANGUAGE TypeFamilies #-}

module ThinkOrSwim.Types where

import           Control.Applicative ((<|>))
import           Control.Lens
import           Data.Foldable (foldl')
import           Data.Ledger as Ledger
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Split
import           Data.Text (Text)
import           Data.Time
import           Prelude hiding (Float, Double, (<>))
import           Text.PrettyPrint
import           ThinkOrSwim.API.TransactionHistory.GetTransactions
                     as API hiding ((<+>))

import qualified Debug.Trace as Trace

renderM :: Applicative f => Doc -> f ()
renderM = Trace.traceM . render

transactionRef :: API.Transaction -> Ref API.Transaction
transactionRef t = Ref OpeningOrder (t^.xactId) (Just t)

lotDate :: CommodityLot API.TransactionSubType API.Transaction -> Maybe Day
lotDate l = l^.purchaseDate
    <|> utctDay <$> l^?refs._head.refOrig._Just.xactDate

daysApart :: CommodityLot API.TransactionSubType API.Transaction -> Day
          -> Maybe Integer
daysApart x yd = do
    xd <- lotDate x
    pure $ xd `diffDays` yd

renderList :: (a -> Doc) -> [a] -> Doc
renderList _ [] = brackets empty
renderList f ts =
    fst (foldl' go (empty, True) ts) <> space <> rbrack
  where
    go (_, True) x    = (lbrack <> space <> f x, False)
    go (acc, False) x = (acc $$ comma <> space <> f x, False)

data GainsKeeperState k t = GainsKeeperState
    { _openTransactions :: Map Text [CommodityLot k t]
    , _positionEvents   :: Map Text [LotAndPL k t]
    }
    deriving (Eq, Ord, Show)

makeLenses ''GainsKeeperState

newGainsKeeperState :: GainsKeeperState k t
newGainsKeeperState = GainsKeeperState
    { _openTransactions = M.empty
    , _positionEvents   = M.empty
    }

-- Return True if 'x' is an open and 'y' is a close.
pairedCommodityLots :: CommodityLot API.TransactionSubType API.Transaction
                    -> CommodityLot API.TransactionSubType API.Transaction
                    -> Bool
pairedCommodityLots x y
    | x' <- x^?Ledger.instrument,
      y' <- y^?Ledger.instrument, x' /= y' = False
    | x^.quantity < 0 && y^.quantity > 0 = True
    | x^.quantity > 0 &&
      (y^.quantity < 0 || y^.kind == OptionExpiration) = True
    | otherwise = False

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Ledger where

import           Control.Lens
import           Data.Amount
import           Data.Default
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import           Data.Time
import           Prelude hiding (Float, Double)

data Instrument
    = Equity
    | Option
    | Future
    | FutureOption
    | Bond
    | MoneyMarket
    deriving (Eq, Ord, Show, Enum, Bounded)

makePrisms ''Instrument

data CommodityLot k = CommodityLot
    { _instrument   :: Instrument
    , _quantity     :: Amount 4
    , _symbol       :: Text
    , _cost         :: Maybe (Amount 4)
    , _purchaseDate :: Maybe Day
    , _note         :: Maybe Text
    , _price        :: Maybe (Amount 4)
    }
    deriving (Eq, Ord, Show)

makeLenses ''CommodityLot

newCommodityLot :: Default k => CommodityLot k
newCommodityLot = CommodityLot
    { _instrument   = Equity
    , _quantity     = 0.0
    , _symbol       = "???"
    , _cost         = Nothing
    , _purchaseDate = Nothing
    , _note         = Nothing
    , _price        = Nothing
    }

data PostingAmount k (lot :: * -> *)
    = NullAmount
    | DollarAmount (Amount 2)
    | CommodityAmount (lot k)
    | MetadataOnly
    deriving (Eq, Ord, Show)

makePrisms ''PostingAmount

data Account
    = Equities Text
    | Futures Text
    | Options Text
    | FuturesOptions Text
    | Forex Text
    | Cash Text
    | Bonds Text
    | MoneyMarkets Text
    | Fees
    | Charges
    | Commissions
    | CapitalGainShort
    | CapitalGainLong
    | CapitalGainCollectible
    | CapitalLossShort
    | CapitalLossLong
    | CapitalLossCollectible
    | CapitalWashLoss
    | CapitalWashLossDeferred
    | RoundingError
    | OpeningBalances
    | Unknown
    deriving (Eq, Ord, Show)

makePrisms ''Account

data Posting k (lot :: * -> *) = Posting
    { _account      :: Account
    , _isVirtual    :: Bool
    , _isBalancing  :: Bool
    , _amount       :: PostingAmount k lot
    , _postMetadata :: Map Text Text
    }
    deriving (Eq, Ord, Show)

makeLenses ''Posting

newPosting :: Account -> Bool -> PostingAmount k lot -> Posting k lot
newPosting a b m = Posting
    { _account      = a
    , _isVirtual    = b
    , _isBalancing  = not b
    , _amount       = m
    , _postMetadata = M.empty
    }

data Transaction k o (lot :: * -> *) = Transaction
    { _actualDate    :: Day
    , _effectiveDate :: Maybe Day
    , _code          :: Text
    , _payee         :: Text
    , _postings      :: [Posting k lot]
    , _xactMetadata  :: Map Text Text
    , _provenance    :: o
    }
    deriving (Eq, Ord, Show)

makeLenses ''Transaction

optionLots :: Traversal' (Transaction k o CommodityLot) (CommodityLot k)
optionLots
    = postings
    . traverse
    . amount
    . _CommodityAmount
    . filtered (has (instrument._Option))

equityLots :: Traversal' (Transaction k o CommodityLot) (CommodityLot k)
equityLots
    = postings
    . traverse
    . amount
    . _CommodityAmount
    . filtered (has (instrument._Equity))

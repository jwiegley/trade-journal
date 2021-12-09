{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Ledger where

import Amount
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time
import Prelude hiding (Double, Float)

data Instrument
  = Equity
  | Option
  | Future
  | FutureOption
  | Bond
  | MoneyMarket
  | Crypto
  deriving (Eq, Ord, Show, Enum, Bounded)

makePrisms ''Instrument

data CommodityLot n = CommodityLot
  { _instrument :: Instrument,
    _quantity :: Amount n,
    _symbol :: Text,
    _cost :: Maybe (Amount n),
    _purchaseDate :: Maybe Day,
    _note :: Maybe Text,
    _price :: Maybe (Amount n)
  }
  deriving (Eq, Ord, Show)

makeLenses ''CommodityLot

newCommodityLot :: CommodityLot n
newCommodityLot =
  CommodityLot
    { _instrument = Equity,
      _quantity = 0.0,
      _symbol = "???",
      _cost = Nothing,
      _purchaseDate = Nothing,
      _note = Nothing,
      _price = Nothing
    }

data PostingAmount n
  = NullAmount
  | DollarAmount (Amount 2)
  | CommodityAmount (CommodityLot n)
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
  | Other Text
  | Unknown
  deriving (Eq, Ord, Show)

makePrisms ''Account

data Posting n = Posting
  { _account :: Account,
    _isVirtual :: Bool,
    _isBalancing :: Bool,
    _amount :: PostingAmount n,
    _postMetadata :: Map Text Text
  }
  deriving (Eq, Ord, Show)

makeLenses ''Posting

newPosting :: Account -> Bool -> PostingAmount n -> Posting n
newPosting a b m =
  Posting
    { _account = a,
      _isVirtual = b,
      _isBalancing = not b,
      _amount = m,
      _postMetadata = M.empty
    }

data Transaction o n = Transaction
  { _actualDate :: Day,
    _effectiveDate :: Maybe Day,
    _code :: Text,
    _payee :: Text,
    _postings :: [Posting n],
    _xactMetadata :: Map Text Text,
    _provenance :: o
  }
  deriving (Eq, Ord, Show)

makeLenses ''Transaction

optionLots :: Traversal' (Transaction o n) (CommodityLot n)
optionLots =
  postings
    . traverse
    . amount
    . _CommodityAmount
    . filtered (has (instrument . _Option))

equityLots :: Traversal' (Transaction o n) (CommodityLot n)
equityLots =
  postings
    . traverse
    . amount
    . _CommodityAmount
    . filtered (has (instrument . _Equity))

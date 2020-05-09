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
import           Data.Int
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import           Data.Time
import           Prelude hiding (Float, Double)

type LotId = Int64

data RefType
    = WashSaleRule (Amount 6)
      -- ^ A wash sale rule increases the cost basis of an equity purchase by
      --   adding previous capital losses, taking those losses off the books.

    | RollingOrder (Amount 6)
      -- ^ In a rolling order, the closing of one option is followed by the
      --   opening of another, and any credit or debit is carried across.
      --
      --   NOTE: GainsKeeper does not do this, and records one as an immediate
      --   loss/gain
    | OpeningOrder
    | ExistingEquity
    deriving (Eq, Ord, Show)

makePrisms ''RefType

data Ref = Ref
    { _refType :: RefType
    , _refId   :: LotId
    }
    deriving (Eq, Ord, Show)

makeLenses ''Ref

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
    , _refs         :: [Ref]
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
    , _refs         = []
    , _price        = Nothing
    }

data PostingAmount k (lot :: * -> *)
    = NoAmount
    | DollarAmount (Amount 2)
    | CommodityAmount (lot k)
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
    | CapitalLossShort
    | CapitalLossLong
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

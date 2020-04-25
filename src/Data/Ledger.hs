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

import           Control.Applicative
import           Control.Lens
import           Data.Amount
import           Data.Coerce
import           Data.Default
import           Data.Int
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import           Data.Time
import           Prelude hiding (Float, Double)

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

data Ref t = Ref
    { _refType :: RefType
    , _refId   :: Int64
    , _refOrig :: Maybe t
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

data CommodityLot k t = CommodityLot
    { _instrument   :: Instrument
    , _kind         :: k
    , _quantity     :: Amount 4
    , _symbol       :: Text
    , _cost         :: Maybe (Amount 4)
    , _purchaseDate :: Maybe Day
    , _refs         :: [Ref t]
    , _price        :: Maybe (Amount 4)
    }
    deriving (Eq, Ord, Show)

makeLenses ''CommodityLot

lotPrice :: CommodityLot k t -> Maybe (Amount 4)
lotPrice l = case l^.instrument of
    Equity  -> sign (l^.quantity) <$> l^.price
        <|> (/ l^.quantity) <$> l^.cost
    Option -> Nothing            -- jww (2020-04-16): NYI
    _      -> Nothing            -- jww (2020-04-16): NYI

newCommodityLot :: Default k => CommodityLot k t
newCommodityLot = CommodityLot
    { _instrument   = Equity
    , _kind         = def
    , _quantity     = 0.0
    , _symbol       = "???"
    , _cost         = Nothing
    , _purchaseDate = Nothing
    , _refs         = []
    , _price        = Nothing
    }

data PL
    = BreakEven
    | GainShort
    | GainLong
    | LossShort
    | LossLong
    | WashLoss
    | Rounding
    deriving (Eq, Ord, Show, Enum, Bounded)

makePrisms ''PL

data LotAndPL k t = LotAndPL
    { _plKind :: PL
    , _plDay  :: Maybe Day
    , _plLoss :: Amount 2           -- positive is loss, else gain or wash
    , _plLot  :: CommodityLot k t
    }
    deriving (Eq, Ord, Show)

makeLenses ''LotAndPL

mkLotAndPL :: CommodityLot k t -> Amount 2 -> CommodityLot k t
           -> LotAndPL k t
mkLotAndPL c pl = LotAndPL knd (c^.purchaseDate) pl
      where
        knd | pl > 0    = LossShort
            | pl < 0    = GainShort
            | otherwise = BreakEven

_Lot :: Prism' (LotAndPL k t) (CommodityLot k t)
_Lot = prism' (\l -> mkLotAndPL l 0 l) (Just . _plLot)

data PostingAmount k t (lot :: * -> * -> *)
    = NoAmount
    | DollarAmount (Amount 2)
    | CommodityAmount (lot k t)
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
    | RoundingError
    | OpeningBalances
    deriving (Eq, Ord, Show)

makePrisms ''Account

plAccount :: PL -> Maybe Account
plAccount BreakEven = Nothing
plAccount GainShort = Just CapitalGainShort
plAccount GainLong  = Just CapitalGainLong
plAccount LossShort = Just CapitalLossShort
plAccount LossLong  = Just CapitalLossLong
plAccount WashLoss  = Just CapitalWashLoss
plAccount Rounding  = Just RoundingError

data Posting k t (lot :: * -> * -> *) = Posting
    { _account      :: Account
    , _isVirtual    :: Bool
    , _isBalancing  :: Bool
    , _amount       :: PostingAmount k t lot
    , _postMetadata :: Map Text Text
    }
    deriving (Eq, Ord, Show)

makeLenses ''Posting

newPosting :: Account -> Bool -> PostingAmount k t lot -> Posting k t lot
newPosting a b m = Posting
    { _account      = a
    , _isVirtual    = b
    , _isBalancing  = not b
    , _amount       = m
    , _postMetadata = M.empty
    }

data Transaction k o t (lot :: * -> * -> *) = Transaction
    { _actualDate    :: Day
    , _effectiveDate :: Maybe Day
    , _code          :: Text
    , _payee         :: Text
    , _postings      :: [Posting k t lot]
    , _xactMetadata  :: Map Text Text
    , _provenance    :: o
    }
    deriving (Eq, Ord, Show)

makeLenses ''Transaction

optionLots :: Traversal' (Transaction k o t LotAndPL) (LotAndPL k t)
optionLots
    = postings
    . traverse
    . amount
    . _CommodityAmount
    . filtered (has (plLot.instrument._Option))

equityLots :: Traversal' (Transaction k o t LotAndPL) (LotAndPL k t)
equityLots
    = postings
    . traverse
    . amount
    . _CommodityAmount
    . filtered (has (plLot.instrument._Equity))

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
{-# LANGUAGE TemplateHaskell #-}

module Data.Ledger where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Amount
import           Data.Char (isAlpha)
import           Data.Coerce
import           Data.Default
import           Data.Int
import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, maybeToList)
import           Data.Split
import           Data.Text (Text, unpack)
import qualified Data.Text as T
import           Data.Time
import           Data.Time.Format.ISO8601
import           Prelude hiding (Float, Double)
import           Text.Printf

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

instance (Eq k, Show k) => Semigroup (CommodityLot k t) where
    x <> y = CommodityLot
        { _instrument   =
          if x^.instrument == y^.instrument
          then y^.instrument
          else error $ "Instrument mismatch: "
                   ++ show (x^.instrument) ++ " != "
                   ++ show (y^.instrument)
        , _kind         =
          if x^.kind == y^.kind
          then y^.kind
          else error $ "Kind mismatch: "
                   ++ show (x^.kind) ++ " != "
                   ++ show (y^.kind)
        , _quantity     = q
        , _symbol       =
          if x^.symbol == y^.symbol
          then y^.symbol
          else error $ "Symbol mismatch: "
                   ++ unpack (x^.symbol) ++ " != "
                   ++ unpack (y^.symbol)
        , _cost         = c
        , _purchaseDate = y^.purchaseDate <|> x^.purchaseDate
        , _refs         = x^.refs ++ y^.refs
        , _price        = do
          lp <- lotPrice x
          rp <- lotPrice y
          pure $ (lp * x^.quantity) + (rp * x^.quantity) / 2
        }
      where
        q = x^.quantity + y^.quantity
        c = liftA2 (+) (sign (x^.quantity) <$> x^.cost)
                       (sign (y^.quantity) <$> y^.cost)

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
_Lot = prism' (\l -> LotAndPL BreakEven (l^.purchaseDate) 0 l)
              (Just . _plLot)

data PostingAmount k t
    = NoAmount
    | DollarAmount (Amount 2)
    | CommodityAmount (CommodityLot k t)
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

data Posting k t = Posting
    { _account      :: Account
    , _isVirtual    :: Bool
    , _isBalancing  :: Bool
    , _amount       :: PostingAmount k t
    , _postMetadata :: Map Text Text
    }
    deriving (Eq, Ord, Show)

makeLenses ''Posting

newPosting :: Account -> Bool -> PostingAmount k t -> Posting k t
newPosting a b m = Posting
    { _account      = a
    , _isVirtual    = b
    , _isBalancing  = not b
    , _amount       = m
    , _postMetadata = M.empty
    }

data Transaction k o t = Transaction
    { _actualDate    :: Day
    , _effectiveDate :: Maybe Day
    , _code          :: Text
    , _payee         :: Text
    , _postings      :: [Posting k t]
    , _xactMetadata  :: Map Text Text
    , _provenance    :: o
    }
    deriving (Eq, Ord, Show)

makeLenses ''Transaction

-- | Check the transaction to ensure that it fully balances. The result is an
--   error string, if an error is detected.
checkTransaction :: Transaction k o t -> Maybe String
checkTransaction _ = Nothing

renderRefs :: [Ref t] -> Text
renderRefs = T.intercalate "," . map go
  where
    go r = case r^.refType of
        WashSaleRule wash ->
            "W$" <> T.pack (show wash) <> "-" <> T.pack (show (r^.refId))
        RollingOrder roll ->
            "R$" <> T.pack (show roll) <> "-" <> T.pack (show (r^.refId))
        OpeningOrder   -> "" <> T.pack (show (r^.refId))
        ExistingEquity -> "Equity"

renderPostingAmount :: PostingAmount k t -> [Text]
renderPostingAmount NoAmount = [""]
renderPostingAmount (DollarAmount amt) = ["$" <> T.pack (thousands amt)]
renderPostingAmount (CommodityAmount l@(CommodityLot {..})) = map T.pack
    [ renderAmount _quantity
    , printf "%s%s%s%s%s"
          (if T.all isAlpha _symbol
           then _symbol
           else "\"" <> _symbol <> "\"")
          (maybe "" (T.pack . printf " {$%s}" . thousands . abs)
                    (perShareCost l))
          (maybe "" (T.pack . printf " [%s]" . iso8601Show) _purchaseDate)
          (case _refs of
               [] -> ""
               xs -> (T.pack . printf " (%s)" . renderRefs) xs)
          (maybe "" (T.pack . printf " @ $%s" . thousands) _price)
    ]
  where
    perShareCost CommodityLot {..} =
        fmap coerce ((/ _quantity) <$> _cost) :: Maybe (Amount 6)

renderAccount :: Account -> Text
renderAccount = \case
    Equities actId       -> "Assets:TD:" <> actId <> ":Equities"
    Futures actId        -> "Assets:TD:" <> actId <> ":Futures"
    Options actId        -> "Assets:TD:" <> actId <> ":Options"
    FuturesOptions actId -> "Assets:TD:" <> actId <> ":Futures:Options"
    Forex actId          -> "Assets:TD:" <> actId <> ":Forex"
    Cash actId           -> "Assets:TD:" <> actId <> ":Cash"
    Bonds actId          -> "Assets:TD:" <> actId <> ":Bonds"
    MoneyMarkets actId   -> "Assets:TD:" <> actId <> ":MoneyMarkets"
    Fees                 -> "Expenses:TD:Fees"
    Charges              -> "Expenses:TD:Charges"
    Commissions          -> "Expenses:TD:Commission"
    CapitalGainShort     -> "Income:Capital:Short"
    CapitalGainLong      -> "Income:Capital:Long"
    CapitalLossShort     -> "Expenses:Capital:Short"
    CapitalLossLong      -> "Expenses:Capital:Long"
    CapitalWashLoss      -> "Expenses:Capital:Short:Wash"
    RoundingError        -> "Expenses:TD:Rounding"
    OpeningBalances      -> "Equity:TD:Opening Balances"

renderPosting :: Posting k t -> [Text]
renderPosting Posting {..} =
    [ T.pack $ printf "    %-32s%16s%s"
        (if _isVirtual then "(" <> act <> ")" else act)
        (head xs)
        (case xs of _:y:_ -> " " <> y; _ -> "")
    ]
    ++ renderMetadata _postMetadata
  where
    act = renderAccount _account
    xs  = renderPostingAmount _amount

renderMetadata :: Map Text Text -> [Text]
renderMetadata = Prelude.map go . M.assocs
  where
    go (k, v) = "    ; " <> k <> ": " <> v

renderTransaction :: Transaction k o t -> [Text]
renderTransaction xact =
    case checkTransaction xact of
        Just err -> error $ "Invalid transaction: " ++ err
        Nothing
            ->  [ T.concat
                   $  [ T.pack (iso8601Show (xact^.actualDate)) ]
                   ++ maybeToList (T.pack . iso8601Show <$> xact^.effectiveDate)
                   ++ [ " * (", xact^.code, ") ", xact^.payee ]
               ]
            ++ renderMetadata (xact^.xactMetadata)
            ++ concatMap renderPosting (xact^.postings)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
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
import           Data.Int
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, maybeToList)
import           Data.Text (Text, unpack)
import qualified Data.Text as T
import           Data.Time
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
    deriving (Eq, Ord, Show)

makePrisms ''RefType

data Ref t = Ref
    { _refType :: RefType
    , _refId   :: Int64
    , _refOrig :: t
    }
    deriving (Eq, Ord, Show)

makeClassy ''Ref

data Instrument
    = Stock
    | Option
    | Future
    | FutureOption
    | Bond
    | MoneyMarket
    deriving (Eq, Ord, Show, Enum, Bounded)

makePrisms ''Instrument

data CommodityLot t = CommodityLot
    { _instrument   :: Instrument
    , _quantity     :: Amount 4
    , _symbol       :: Text
    , _cost         :: Maybe (Amount 4)
    , _purchaseDate :: Maybe UTCTime
    , _refs         :: [Ref t]
    , _price        :: Maybe (Amount 4)
    }
    deriving (Eq, Ord, Show)

makeClassy ''CommodityLot

instance Semigroup (CommodityLot t) where
    x <> y = CommodityLot
        { _instrument   =
          if x^.instrument == y^.instrument
          then y^.instrument
          else error $ "Instrument mismatch: "
                   ++ show (x^.instrument) ++ " != "
                   ++ show (y^.instrument)
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
        , _price        = (/ q) <$> c
        }
      where
        q = x^.quantity + y^.quantity
        c = liftA2 (+) (sign x <$> x^.cost)
                       (sign y <$> y^.cost)

instance Monoid (CommodityLot t) where
    mempty  = newCommodityLot
    mappend = (<>)

lotPrice :: CommodityLot t -> Maybe (Amount 4)
lotPrice l = do
    guard $ l^.instrument == Stock
    p <- l^.price
    pure $ if l^.quantity < 0 then (-p) else p

lotCost :: CommodityLot t -> Amount 4
lotCost l = fromMaybe 0.0 (l^.cost <|> ((l^.quantity) *) <$> lotPrice l)

data LotSplit t
    = Some
        { _used      :: CommodityLot t
        , _remaining :: CommodityLot t
        }
    | All (CommodityLot t)
    | None
    deriving (Eq, Ord, Show)

_SplitLeft :: Traversal' (LotSplit t) (CommodityLot t)
_SplitLeft f (Some u r) = Some <$> f u <*> pure r
_SplitLeft f (All u)    = All <$> f u
_SplitLeft _ None       = pure None

_SplitRight :: Traversal' (LotSplit t) (CommodityLot t)
_SplitRight f (Some u r) = Some u <$> f r
_SplitRight _ (All u)    = pure $ All u
_SplitRight _ None       = pure None

showLotSplit :: LotSplit t -> String
showLotSplit None       = "None"
showLotSplit (All l)    = "All (" ++ showCommodityLot l ++ ")"
showLotSplit (Some l r) =
    "Some (" ++ showCommodityLot l ++ ") (" ++ showCommodityLot r ++ ")"

alignLots :: CommodityLot t -> CommodityLot t -> (LotSplit t, LotSplit t)
alignLots x y
    | xq == 0 && yq == 0 = ( None,  None  )
    | xq == 0           = ( None,  All y )
    | yq == 0           = ( All x, None  )
    | abs xq == abs yq  = ( All x, All y )
    | abs xq <  abs yq  =
        ( All x
        , Some (y & quantity .~ sign y xq
                  & cost     ?~ abs xq * yps)
               (y & quantity .~ sign y diff
                  & cost     ?~ diff * yps)
        )
    | otherwise =
        ( Some (x & quantity .~ sign x yq
                  & cost     ?~ abs yq * xps)
               (x & quantity .~ sign x diff
                  & cost     ?~ diff * xps)
        , All y
        )
  where
    xq    = x^.quantity
    yq    = y^.quantity
    xcst  = lotCost x
    ycst  = lotCost y
    xps   = xcst / abs xq
    yps   = ycst / abs yq
    diff  = abs (abs xq - abs yq)

sign :: Num a => CommodityLot t -> a -> a
sign l = (if l^.quantity < 0 then negate else id) . abs

newCommodityLot :: CommodityLot t
newCommodityLot = CommodityLot
    { _instrument   = Stock
    , _quantity     = 0.0
    , _symbol       = "???"
    , _cost         = Nothing
    , _purchaseDate = Nothing
    , _refs         = []
    , _price        = Nothing
    }

(@@) :: Amount 4 -> Amount 4 -> CommodityLot t
q @@ c = newCommodityLot & quantity .~ q & cost ?~ c

showCommodityLot :: CommodityLot t -> String
showCommodityLot CommodityLot {..} =
    show _quantity
        ++ case _cost of
               Nothing -> ""
               Just xs -> " @@ " ++ show xs

data PostingAmount t
    = NoAmount
    | DollarAmount (Amount 2)
    | CommodityAmount (CommodityLot t)
    deriving (Eq, Ord, Show)

makePrisms ''Amount

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
    | RoundingError
    | OpeningBalances
    deriving (Eq, Ord, Show)

makePrisms ''Account

data Posting t = Posting
    { _account      :: Account
    , _isVirtual    :: Bool
    , _isBalancing  :: Bool
    , _amount       :: PostingAmount t
    , _postMetadata :: Map Text Text
    }
    deriving (Eq, Ord, Show)

makeClassy ''Posting

data Transaction o t = Transaction
    { _actualDate    :: UTCTime
    , _effectiveDate :: Maybe UTCTime
    , _code          :: Text
    , _payee         :: Text
    , _postings      :: [Posting t]
    , _xactMetadata  :: Map Text Text
    , _provenance    :: o
    }
    deriving (Eq, Ord, Show)

makeClassy ''Transaction

-- | Check the transaction to ensure that it fully balances. The result is an
--   error string, if an error is detected.
checkTransaction :: Transaction o t -> Maybe String
checkTransaction _ = Nothing

renderRefs :: [Ref t] -> Text
renderRefs = T.intercalate "," . map go
  where
    go r = (case r^.refType of
                WashSaleRule wash -> "WASH[$" <> T.pack (show wash) <> "]:"
                RollingOrder roll -> "ROLL[$" <> T.pack (show roll) <> "]:"
                OpeningOrder      -> "") <> T.pack (show (r^.refId))

renderPostingAmount :: PostingAmount t -> [Text]
-- jww (2020-03-29): Need to add commas, properly truncate, etc.
renderPostingAmount NoAmount = [""]
renderPostingAmount (DollarAmount amt) = ["$" <> T.pack (thousands amt)]
renderPostingAmount (CommodityAmount CommodityLot {..}) =
    [ T.pack (renderAmount _quantity)
    , T.pack $ printf "%s%s%s%s%s"
          (if T.all isAlpha _symbol then _symbol else "\"" <> _symbol <> "\"")
          -- (maybe "" (T.pack . printf " {{$%s}}" . thousands . abs) _cost)
          (maybe "" (T.pack . printf " {$%s}" . thousands . abs)
                    (fmap coerce ((/ _quantity) <$> _cost) :: Maybe (Amount 6)))
          (maybe "" (T.pack . printf " [%s]" . iso8601) _purchaseDate)
          (case _refs of [] -> ""; xs -> (T.pack . printf " (%s)" . renderRefs) xs)
          (maybe "" (T.pack . printf " @ $%s" . thousands) _price)
    ]

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
    RoundingError        -> "Expenses:TD:Rounding"
    OpeningBalances      -> "Equity:TD:Opening Balances"

renderPosting :: Posting t -> [Text]
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

renderTransaction :: Transaction o t -> [Text]
renderTransaction xact =
    case checkTransaction xact of
        Just err -> error $ "Invalid transaction: " ++ err
        Nothing
            ->  [ T.concat
                   $  [ iso8601 (xact^.actualDate) ]
                   ++ maybeToList (iso8601 <$> xact^.effectiveDate)
                   ++ [ " * (", xact^.code, ") ", xact^.payee ]
               ]
            ++ renderMetadata (xact^.xactMetadata)
            ++ concatMap renderPosting (xact^.postings)

iso8601 :: UTCTime -> Text
iso8601 = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

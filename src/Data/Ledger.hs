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
        c = liftA2 (+) (sign x <$> x^.cost)
                       (sign y <$> y^.cost)

lotPrice :: CommodityLot k t -> Maybe (Amount 4)
lotPrice l = case l^.instrument of
    Equity  -> sign l <$> l^.price
        <|> (/ l^.quantity) <$> l^.cost
    Option -> Nothing            -- jww (2020-04-16): NYI
    _      -> Nothing            -- jww (2020-04-16): NYI

lotCost :: CommodityLot k t -> Amount 4
lotCost l = fromMaybe 0.0 (l^.cost <|> ((l^.quantity) *) <$> lotPrice l)

showSplit :: Split (CommodityLot k t) -> String
showSplit (None k)   = "None (" ++ showCommodityLot k ++ ")"
showSplit (All u)    = "All (" ++ showCommodityLot u ++ ")"
showSplit (Some u k) =
    "Some (" ++ showCommodityLot u ++ ") (" ++ showCommodityLot k ++ ")"

alignLots :: CommodityLot k t -> CommodityLot k t
          -> (Split (CommodityLot k t), Split (CommodityLot k t))
alignLots x y
    | xq == 0 && yq == 0 = ( None x, None y )
    | xq == 0           = ( None x, All  y )
    | yq == 0           = ( All  x, None y )
    | abs xq == abs yq  = ( All  x, All  y )
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

sign :: Num a => CommodityLot k t -> a -> a
sign l = (if l^.quantity < 0 then negate else id) . abs

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

(@@) :: Default k => Amount 4 -> Amount 4 -> CommodityLot k t
q @@ c = newCommodityLot & quantity .~ q & cost ?~ c

(##) :: CommodityLot k t -> Text -> CommodityLot k t
l ## d = l & purchaseDate .~ iso8601ParseM (T.unpack d)

showCommodityLot :: CommodityLot k t -> String
showCommodityLot CommodityLot {..} =
    show _quantity
        ++ case _cost of
               Nothing -> ""
               Just xs -> " @@ " ++ show xs
        ++ case _purchaseDate of
               Nothing -> ""
               Just d  -> " ## " ++ iso8601Show d

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

_Lot :: Prism' (LotAndPL k t) (CommodityLot k t)
_Lot = prism' (LotAndPL BreakEven Nothing 0) (Just . _plLot)

-- instance Show (LotAndPL k t) where
showLotAndPL :: LotAndPL k t -> String
showLotAndPL x = show (x^.plKind)
    ++ " " ++ showCommodityLot (x^.plLot)
    ++ " $$$ "  ++ show (x^.plLoss)

($$$) :: CommodityLot k t -> Amount 2 -> LotAndPL k t
l $$$ a = LotAndPL (if | a < 0     -> GainShort
                       | a > 0     -> LossShort
                       | otherwise -> BreakEven) (l^.purchaseDate) a l

alignPL :: LotAndPL k t -> LotAndPL k t
        -> (Split (LotAndPL k t), Split (LotAndPL k t))
alignPL x y =
    ( l & unsafePartsOf _Splits
       %~ fmap (uncurry (LotAndPL (x^.plKind) (x^.plDay)))
        . spreadAmounts (^.quantity) (x^.plLoss)
    , r & unsafePartsOf _Splits
       %~ fmap (uncurry (LotAndPL (y^.plKind) (y^.plDay)))
        . spreadAmounts (^.quantity) (y^.plLoss)
    )
  where
    (l, r) = (x^.plLot) `alignLots` (y^.plLot)

perShareCost :: CommodityLot k t -> Maybe (Amount 6)
perShareCost CommodityLot {..} =
    fmap coerce ((/ _quantity) <$> _cost) :: Maybe (Amount 6)

-- The idea of this function is to replicate what Ledger will calculate the
-- sum to be, so that if there's any discrepancy we can add a rounding
-- adjustment to pass the balancing check.
sumLotAndPL :: [LotAndPL k t] -> Amount 2
sumLotAndPL = foldl' go 0
  where
    norm      = normalizeAmount mpfr_RNDNA
    cst l     = sign l (fromMaybe 0 (l^.cost))
    go acc pl = acc + norm (coerce (cst (pl^.plLot))) + norm (pl^.plLoss)

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

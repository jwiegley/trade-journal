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
import           Data.Char (isAlpha)
import           Data.Either (isRight)
import           Data.Int
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, maybeToList)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Numeric.Rounded
import           Text.Printf

data RefType
    = WashSaleRule Double
      -- ^ A wash sale rule increases the cost basis of an equity purchase by
      --   adding previous capital losses, taking those losses off the books.

    | RollingOrder Double
      -- ^ In a rolling order, the closing of one option is followed by the
      --   opening of another, and any credit or debit is carried across.
      --
      --   NOTE: GainsKeeper does not do this, and records one as an immediate
      --   loss/gain
    | OpeningOrder
    deriving (Eq, Ord, Show)

makePrisms ''RefType

data Ref = Ref
    { _refType :: RefType
    , _refId   :: Int64
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
    deriving (Eq, Ord, Show, Enum)

makePrisms ''Instrument

data CommodityLot = CommodityLot
    { _instrument   :: Instrument
    , _quantity     :: Double
    , _symbol       :: Text
    , _cost         :: Maybe Double
    , _purchaseDate :: Maybe UTCTime
    , _refs         :: [Ref]
    , _price        :: Maybe Double
    }
    deriving (Eq, Ord, Show)

makeClassy ''CommodityLot

lotPrice :: CommodityLot -> Maybe Double
lotPrice l = do
    guard $ l^.instrument `notElem` [ Option, FutureOption ]
    p <- l^.price
    pure $ if l^.quantity < 0 then (-p) else p

lotCost :: CommodityLot -> Double
lotCost l = fromMaybe 0.0 (l^.cost <|> ((l^.quantity) *) <$> lotPrice l)

newCommodityLot :: CommodityLot
newCommodityLot = CommodityLot
    { _instrument   = Stock
    , _quantity     = 0.0
    , _symbol       = "???"
    , _cost         = Nothing
    , _purchaseDate = Nothing
    , _refs         = []
    , _price        = Nothing
    }

showCommodityLot :: CommodityLot -> String
showCommodityLot CommodityLot {..} =
    show _quantity ++ " @@ " ++ show _cost

data Amount
    = NoAmount
    | DollarAmount Double
    | CommodityAmount CommodityLot
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

data Posting = Posting
    { _account      :: Account
    , _isVirtual    :: Bool
    , _isBalancing  :: Bool
    , _amount       :: Amount
    , _postMetadata :: Map Text Text
    }
    deriving (Eq, Ord, Show)

makeClassy ''Posting

data Transaction p = Transaction
    { _actualDate    :: UTCTime
    , _effectiveDate :: Maybe UTCTime
    , _code          :: Text
    , _payee         :: Text
    , _postings      :: [Posting]
    , _xactMetadata  :: Map Text Text
    , _provenance    :: p
    }
    deriving (Eq, Ord, Show)

makeClassy ''Transaction

-- | Check the transaction to ensure that it fully balances. The result is an
--   error string, if an error is detected.
checkTransaction :: Transaction p -> Maybe String
checkTransaction _ = Nothing

renderRefs :: [Ref] -> Text
renderRefs = T.intercalate "," . map go
  where
    go r = (case r^.refType of
                WashSaleRule wash -> "WASH[$" <> doubleToText 2 wash <> "]:"
                RollingOrder roll -> "ROLL[$" <> doubleToText 2 roll <> "]:"
                OpeningOrder      -> "") <> T.pack (show (r^.refId))

roundTo :: Int -> Double -> Double
-- roundTo n x = fromIntegral (round (x * (10^n)) :: Int) / 10^n
roundTo n x = toDouble
    (fromInteger
     (toInteger'
      (fromDouble x * (10^n) :: Rounded 'TowardNearest 128))
        / 10^n :: Rounded 'TowardNearest 128)

doubleToText :: Int -> Double -> Text
doubleToText n =
    cleanup 2 . T.pack . printf ("%0." ++ show n ++ "f") . roundTo n
  where
    cleanup m t =
        let len = T.length (last (T.splitOn "." t)) in
        if len > m && T.last t == '0'
        then cleanup m (T.take (T.length t - 1) t)
        else t

thousands :: Int -> Either Int Double -> Text
thousands n d = T.intercalate "." $
    case T.splitOn "." str of
        x:xs -> (T.pack . reverse . go . reverse . T.unpack) x :
            case xs of
                y:ys -> (T.pack . expand . T.unpack) y : ys
                _ | isRight d -> ["00"]
                _ -> []
        xs -> xs
  where
    str = case d of
        Right f -> doubleToText n f
        Left  i -> T.pack (show i)

    go (x:y:z:[])    = x:y:z:[]
    go (x:y:z:['-']) = x:y:z:['-']
    go (x:y:z:xs)    = x:y:z:',':go xs
    go xs            = xs

    expand []     = "00"
    expand (x:[]) = x:"0"
    expand xs     = xs

renderDouble :: Double -> Text
renderDouble d | fromIntegral (floor d :: Int) == d =
    thousands 2 (Left (floor d :: Int))
renderDouble d = thousands 2 (Right d)

renderAmount :: Amount -> [Text]
-- jww (2020-03-29): Need to add commas, properly truncate, etc.
renderAmount NoAmount = [""]
renderAmount (DollarAmount amt) = ["$" <> thousands 2 (Right amt)]
renderAmount (CommodityAmount CommodityLot {..}) =
    [ renderDouble _quantity
    , T.pack $ printf "%s%s%s%s%s"
          (if T.all isAlpha _symbol then _symbol else "\"" <> _symbol <> "\"")
          (maybe "" (T.pack . printf " {{$%s}}" . thousands 6 . Right . abs) _cost)
          (maybe "" (T.pack . printf " [%s]" . iso8601) _purchaseDate)
          (case _refs of [] -> ""; xs -> (T.pack . printf " (%s)" . renderRefs) xs)
          (maybe "" (T.pack . printf " @ $%s" . thousands 6 . Right) _price)
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

renderPosting :: Posting -> [Text]
renderPosting Posting {..} =
    [ T.pack $ printf "    %-32s%16s%s"
        (if _isVirtual then "(" <> act <> ")" else act)
        (head xs)
        (case xs of _:y:_ -> " " <> y; _ -> "")
    ]
    ++ renderMetadata _postMetadata
  where
    act = renderAccount _account
    xs  = renderAmount _amount

renderMetadata :: Map Text Text -> [Text]
renderMetadata = Prelude.map go . M.assocs
  where
    go (k, v) = "    ; " <> k <> ": " <> v

renderTransaction :: Transaction p -> [Text]
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

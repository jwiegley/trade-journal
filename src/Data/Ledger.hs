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

import           Control.Lens
import           Data.Char (isAlpha)
import           Data.Either (isRight)
import           Data.Int
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (maybeToList)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
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
    deriving (Eq, Ord, Show, Enum)

data Amount
    = NoAmount
    | DollarAmount Double
    | CommodityAmount
      { _instrument   :: Instrument
      , _quantity     :: Double
      , _symbol       :: Text
      , _cost         :: Maybe Double
      , _purchaseDate :: Maybe UTCTime
      , _refs         :: [Ref]
      , _price        :: Maybe Double
      }
    deriving (Eq, Ord, Show)

makePrisms ''Amount

data Account
    = Equities Text
    | Futures Text
    | Options Text
    | FuturesOptions Text
    | Forex Text
    | Cash Text
    | Fees
    | Charges
    | Commissions
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

doubleToText :: Int -> Double -> Text
doubleToText n = cleanup . T.pack . printf "%0.6f"
  where
    cleanup t =
        let len = T.length (last (T.splitOn "." t)) in
        if len > n && T.last t == '0'
        then cleanup (T.take (T.length t - 1) t)
        else t

thousands :: Either Int Double -> Text
thousands d = T.intercalate "." $
    case T.splitOn "." str of
        x:xs -> (T.pack . reverse . go . reverse . T.unpack) x :
            case xs of
                y:ys -> (T.pack . expand . T.unpack) y : ys
                _ | isRight d -> ["00"]
                _ -> []
        xs -> xs
  where
    str = case d of
        Right f -> doubleToText 2 f
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
    thousands (Left (floor d :: Int))
renderDouble d = thousands (Right d)

renderAmount :: Amount -> [Text]
-- jww (2020-03-29): Need to add commas, properly truncate, etc.
renderAmount NoAmount = [""]
renderAmount (DollarAmount amt) = ["$" <> thousands (Right amt)]
renderAmount CommodityAmount {..} =
    [ renderDouble _quantity
    , T.pack $ printf "%s%s%s%s%s"
          (if T.all isAlpha _symbol then _symbol else "\"" <> _symbol <> "\"")
          (maybe "" (T.pack . printf " {{$%s}}" . thousands . Right) _cost)
          (maybe "" (T.pack . printf " [%s]" . iso8601) _purchaseDate)
          (case _refs of [] -> ""; xs -> (T.pack . printf " (%s)" . renderRefs) xs)
          (maybe "" (T.pack . printf " @ $%s" . thousands . Right) _price)
    ]

renderAccount :: Account -> Text
renderAccount = \case
    Equities actId       -> "Assets:TD:" <> actId <> ":Equities"
    Futures actId        -> "Assets:TD:" <> actId <> ":Futures"
    Options actId        -> "Assets:TD:" <> actId <> ":Options"
    FuturesOptions actId -> "Assets:TD:" <> actId <> ":Futures:Options"
    Forex actId          -> "Assets:TD:" <> actId <> ":Forex"
    Cash actId           -> "Assets:TD:" <> actId <> ":Cash"
    Fees                 -> "Expenses:TD:Fees"
    Charges              -> "Expenses:TD:Charges"
    Commissions          -> "Expenses:TD:Commission"
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

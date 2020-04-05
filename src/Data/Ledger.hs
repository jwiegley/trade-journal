{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Ledger where

import           Control.Lens
import           Data.Map (Map)
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
    deriving (Eq, Ord, Show)

makePrisms ''RefType

data Ref = Ref
    { _refType :: RefType
    , _refId   :: Text
    }
    deriving (Eq, Ord, Show)

makeClassy ''Ref

data Amount
    = DollarAmount Double
    | CommodityAmount
      { _quantity     :: Double
      , _symbol       :: Text
      , _cost         :: Maybe Double
      , _purchaseDate :: Maybe UTCTime
      , _refs         :: Maybe [Ref]
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
    | Commissions
    deriving (Eq, Ord, Show)

makePrisms ''Account

data Posting = Posting
    { _account     :: Account
    , _isVirtual   :: Bool
    , _isBalancing :: Bool
    , _amount      :: Amount
    }
    deriving (Eq, Ord, Show)

makeClassy ''Posting

data Transaction p = Transaction
    { _actualDate    :: UTCTime
    , _effectiveDate :: Maybe UTCTime
    , _code          :: Text
    , _payee         :: Text
    , _postings      :: [Posting]
    , _metadata      :: Map Text Text
    , _provenance    :: p
    }
    deriving (Eq, Ord, Show)

makeClassy ''Transaction

-- | Check the transaction to ensure that it fully balances. The result is an
--   error string, if an error is detected.
checkTransaction :: Transaction p -> Maybe String
checkTransaction _ = Nothing

renderAmount :: Amount -> Text
-- jww (2020-03-29): Need to add commas, properly truncate, etc.
renderAmount (DollarAmount x) = T.pack $ printf "$%.02f" x
renderAmount CommodityAmount {..} = ""
    -- T.concat $ L.intersperse " " [
    -- T.pack $ printf "%s %s {%s} [%s] () @ %s" _quantity _symbol _cost _purchaseDate (show _refs) _price

renderAmountSuffix :: Amount -> Text
renderAmountSuffix _ = ""

renderAccount :: Account -> Text
renderAccount _ = "Account"

renderPosting :: Posting -> Text
renderPosting Posting {..} =
    T.pack $ printf "    %32s%-16s%s"
        (if _isVirtual then "(" <> act <> ")" else act)
        (renderAmount _amount)
        (renderAmountSuffix _amount)
  where
    act = renderAccount _account

renderMetadata :: Map Text Text -> [Text]
renderMetadata _m = []

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
            ++ renderMetadata (xact^.metadata)
            ++ map renderPosting (xact^.postings)
  where
    iso8601 = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

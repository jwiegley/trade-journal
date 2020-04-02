{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Ledger where

import           Control.Lens
import           Data.Map (Map)
-- import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Text.Printf
import           Unsafe.Coerce (unsafeCoerce)

data RefType
    = WashSaleRule Double
    | RollingOrder Double
    deriving (Eq, Show, Ord)

makePrisms ''RefType

data Ref = Ref
    { _refType :: RefType
    , _refId   :: Text
    }
    deriving Show

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
    deriving Show

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
    deriving (Eq, Show, Ord)

makePrisms ''Account

-- A pending transaction is a Ledger transaction that pretty much exactly
-- reflects the downloaded order from TD Ameritrade. A final transaction has
-- had its cost basis -- and thus, gains or losses -- calculated by reviewing
-- the history of all prior transaction, to determine what previous positions
-- are being closed, whether the wash sale rule applies, etc. The final
-- transaction should represent "reality", and tally with any taxable gains
-- reports.
data DataState = Pending | Final
    deriving (Eq, Show, Ord, Enum)

data Posting (s :: DataState) = Posting
    { _account   :: Account
    , _isVirtual :: Bool
    , _amount    :: Amount
    }
    deriving Show

makeClassy ''Posting

data Transaction p (s :: DataState) = Transaction
    { _date       :: UTCTime
    , _code       :: Text
    , _payee      :: Text
    , _postings   :: [Posting s]
    , _metadata   :: Map Text Text
    , _provenance :: [p]
    }
    deriving Show

makeClassy ''Transaction

-- | Check the transaction to ensure that it fully balances. The result is an
--   error string, if an error is detected.
checkTransaction :: Transaction p 'Final -> Maybe String
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

renderPosting :: Posting 'Final -> Text
renderPosting Posting {..} =
    T.pack $ printf "    %32s%-16s%s"
        (if _isVirtual then "(" <> act <> ")" else act)
        (renderAmount _amount)
        (renderAmountSuffix _amount)
  where
    act = renderAccount _account

renderMetadata :: Map Text Text -> [Text]
renderMetadata _m = []

renderTransaction :: Transaction p 'Final -> [Text]
renderTransaction xact@Transaction {..} =
    case checkTransaction xact of
        Just err -> error $ "Invalid transaction: " ++ err
        Nothing -> [ moment <> " * (" <> _code <> ") " <> _payee ]
            ++ renderMetadata _metadata
            ++ map renderPosting _postings
  where
    moment = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" _date

finalizeTransactions :: [Transaction p 'Final] -> [Transaction p 'Pending]
                     -> [Transaction p 'Final]
finalizeTransactions _old new = unsafeCoerce new

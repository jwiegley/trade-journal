{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Ledger where

import           Control.Lens
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Text.Printf

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
      , _cost         :: Double
      , _purchaseDate :: UTCTime
      , _refs         :: [Ref]
      , _price        :: Double
      }
    deriving Show

makePrisms ''Amount

data Account
    = Equities
    | Futures
    | Options
    | FuturesOptions
    | Forex
    deriving (Eq, Show, Ord, Enum)

makePrisms ''Account

data Posting = Posting
    { _accountId  :: Text
    , _subAccount :: Account
    , _isVirtual  :: Bool
    , _amount     :: Amount
    }
    deriving Show

makeClassy ''Posting

data Transaction = Transaction
    { _date     :: UTCTime
    , _code     :: Text
    , _payee    :: Text
    , _postings :: [Posting]
    , _metadata :: Map Text Text
    }
    deriving Show

makeClassy ''Transaction

-- | Check the transaction to ensure that it fully balances. The result is an
--   error string, if an error is detected.
checkTransaction :: Transaction -> Maybe String
checkTransaction _ = Nothing

renderAmount :: Amount -> Text
renderAmount _ = ""

renderAmountSuffix :: Amount -> Text
renderAmountSuffix _ = ""

renderAccount :: Text -> Account -> Text
renderAccount _ _ = ""

renderPosting :: Text -> Posting -> Text
renderPosting actId post@Posting {..} =
    T.pack $ printf "    %32s%-16s%s"
        (if _isVirtual then "(" <> act <> ")" else act)
        (renderAmount _amount)
        (renderAmountSuffix _amount)
  where
    act = renderAccount actId _subAccount

renderMetadata :: Map Text Text -> [Text]
renderMetadata _m = []

renderTransaction :: Text -> Transaction -> Text
renderTransaction actId xact@Transaction {..} =
    case checkTransaction xact of
        Just err -> error $ "Invalid transaction: " ++ err
        Nothing ->
            T.concat $ [ moment <> " * (" <> _code <> ") " <> _payee <> "\n" ]
                ++ renderMetadata _metadata
                ++ map (renderPosting actId) _postings
  where
    moment = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" _date

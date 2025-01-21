{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Trade.Provider.Coinbase.Types where

import Amount
import Control.Lens
import Data.Csv ((.:))
import Data.Csv qualified as Csv
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.Time.Format.ISO8601
import GHC.Generics
import GHC.TypeLits (KnownNat)
import Text.Read (readMaybe)

data Transaction = Transaction
  { _xactID :: !Text,
    _xactTimestamp :: !UTCTime,
    _xactTransactionType :: !Text,
    _xactAsset :: !Text,
    _xactQuantityTransacted :: !(Amount 2),
    _xactPriceCurrency :: !Text,
    _xactPriceAtTransaction :: !(Amount 2),
    _xactSubtotal :: !(Amount 2),
    _xactTotal :: !(Amount 2),
    _xactFeesAndOrSpread :: !(Amount 2),
    _xactNotes :: !Text
  }
  deriving (Generic, Eq, Show)

readAmount :: (KnownNat n) => String -> Amount n
readAmount "" = 0
readAmount ('-' : xs) = -(readAmount xs)
readAmount s = case readMaybe (filter (`notElem` [',', '$', ')']) s) of
  Nothing -> error $ "Failed to read amount: " ++ s
  Just x -> x

instance Csv.FromNamedRecord Transaction where
  parseNamedRecord m =
    Transaction
      <$> (m .: "ID")
      <*> ( fromMaybe
              ( error
                  "Could not parse time"
              )
              . iso8601ParseM
              . T.unpack
              <$> m .: "Timestamp"
          )
      <*> (m .: "Transaction Type")
      <*> (m .: "Asset")
      <*> (readAmount <$> m .: "Quantity Transacted")
      <*> (m .: "Price Currency")
      <*> (readAmount <$> m .: "Price at Transaction")
      <*> (readAmount <$> m .: "Subtotal")
      <*> (readAmount <$> m .: "Total (inclusive of fees and/or spread)")
      <*> (readAmount <$> m .: "Fees and/or Spread")
      <*> (m .: "Notes")

makeLenses ''Transaction

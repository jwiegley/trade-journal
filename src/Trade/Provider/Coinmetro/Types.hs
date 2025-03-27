{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Trade.Provider.Coinmetro.Types where

import Amount
import Control.Lens
import Data.Csv ((.:))
import Data.Csv qualified as Csv
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import GHC.Generics
import GHC.TypeLits (KnownNat)
import Text.Read (readMaybe)

data Transaction = Transaction
  { _xactAsset :: !Text,
    _xactDate :: !UTCTime,
    _xactDescription :: !Text,
    _xactAmount :: !(Amount 2),
    _xactFee :: !(Amount 2),
    _xactPrice :: !(Either Text (Amount 2)),
    _xactPair :: !Text,
    _xactOtherCurrency :: !Text,
    _xactOtherAmount :: !(Either Text (Amount 2)),
    _xactIBAN :: !Text,
    _xactTransactionHash :: !Text,
    _xactAddress :: !Text,
    _xactTram :: !Text,
    _xactAdditionalInfo :: !Text,
    _xactReferenceNote :: !Text,
    _xactComment :: !Text
  }
  deriving (Generic, Eq, Show)

readAmount :: (KnownNat n) => String -> Amount n
readAmount "" = 0
readAmount ('-' : xs) = -(readAmount xs)
readAmount s = case readMaybe (filter (`notElem` [',', '$', ')']) s) of
  Nothing -> error $ "Failed to read amount: " ++ s
  Just x -> x

readAmountMaybe :: (KnownNat n) => String -> Either Text (Amount n)
readAmountMaybe s@"n/a" = Left (T.pack s)
readAmountMaybe s = Right (readAmount s)

instance Csv.FromNamedRecord Transaction where
  parseNamedRecord m =
    Transaction
      <$> (m .: "Asset")
      <*> ( do
              date <- m .: "Date"
              case parseTimeM
                False
                defaultTimeLocale
                "%Y-%m-%d %H:%M:%S"
                date of
                Nothing -> error $ "Could not parse time '" ++ date ++ "'"
                Just d -> pure d
          )
      <*> (m .: "Description")
      <*> (readAmount <$> m .: "Amount")
      <*> (readAmount <$> m .: "Fee")
      <*> (readAmountMaybe <$> m .: "Price")
      <*> (m .: "Pair")
      <*> (m .: "Other Currency")
      <*> (readAmountMaybe <$> m .: "Other Amount")
      <*> (m .: "IBAN")
      <*> (m .: "Transaction Hash")
      <*> (m .: "Address")
      <*> (m .: "Tram")
      <*> (m .: "Additional Info")
      <*> (m .: "Reference Note")
      <*> (m .: "Comment")

makeLenses ''Transaction

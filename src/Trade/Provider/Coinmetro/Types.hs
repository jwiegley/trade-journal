{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Trade.Provider.Coinmetro.Types where

import Amount
import Control.Lens
import Data.Csv ((.:))
import qualified Data.Csv as Csv
import Data.Text (Text)
import GHC.Generics
import GHC.TypeLits (KnownNat)
import Text.Read (readMaybe)

data Transaction = Transaction
  { _xactAsset :: !Text,
    _xactDate :: !Text,
    _xactDescription :: !Text,
    _xactAmount :: !(Amount 2),
    _xactFee :: !(Amount 2),
    _xactPrice :: !(Amount 2),
    _xactPair :: !Text,
    _xactOtherCurrency :: !Text,
    _xactOtherAmount :: !(Amount 2),
    _xactIBAN :: !Text,
    _xactTransactionHash :: !Text,
    _xactAddress :: !Text,
    _xactTram :: !Text,
    _xactAdditionalInfo :: !Text,
    _xactReferenceNote :: !Text,
    _xactComment :: !Text
  }
  deriving (Generic, Eq, Show)

readAmount :: KnownNat n => String -> Amount n
readAmount "" = 0
readAmount ('-' : xs) = -(readAmount xs)
readAmount s = case readMaybe (filter (`notElem` [',', '$', ')']) s) of
  Nothing -> error $ "Failed to read amount: " ++ s
  Just x -> x

instance Csv.FromNamedRecord Transaction where
  parseNamedRecord m =
    Transaction
      <$> (m .: "Asset")
      <*> (m .: "Date")
      <*> (m .: "Description")
      <*> (readAmount <$> m .: "Amount")
      <*> (readAmount <$> m .: "Fee")
      <*> (readAmount <$> m .: "Price")
      <*> (m .: "Pair")
      <*> (m .: "Other Currency")
      <*> (readAmount <$> m .: "Other Amount")
      <*> (m .: "IBAN")
      <*> (m .: "Transaction Hash")
      <*> (m .: "Address")
      <*> (m .: "Tram")
      <*> (m .: "Additional Info")
      <*> (m .: "Reference Note")
      <*> (m .: "Comment")

makeLenses ''Transaction

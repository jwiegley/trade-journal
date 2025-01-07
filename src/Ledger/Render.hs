{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ledger.Render where

import Amount
import Control.Applicative
import Control.Lens
import Data.Char (isAlpha)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Format.ISO8601
import GHC.TypeLits
import Ledger
import Text.Printf
import Prelude hiding (Double, Float)

renderPostingAmount :: (KnownNat n) => PostingAmount n -> NonEmpty Text
renderPostingAmount MetadataOnly = error "unexpected MetadataOnly"
renderPostingAmount NullAmount = "" :| []
renderPostingAmount (DollarAmount amt) = "$" <> T.pack (thousands amt) :| []
renderPostingAmount (CommodityAmount CommodityLot {..}) =
  T.pack (renderAmount _quantity) :| [T.pack rendered]
  where
    rendered :: String
    rendered =
      printf
        "%s%s%s%s%s"
        ( if T.all isAlpha _symbol
            then _symbol
            else "\"" <> _symbol <> "\""
        )
        ( maybe
            ""
            (T.pack . printf " {$%s}" . thousands . abs)
            _cost
        )
        (maybe "" (T.pack . printf " [%s]" . iso8601Show) _purchaseDate)
        (maybe "" (T.pack . printf " (%s)") _note)
        (maybe "" (T.pack . printf " @ $%s" . thousands) _price)

renderAccount :: Text -> Account -> Text
renderAccount name = \case
  Account "" -> "Assets:" <> name
  Account actId -> "Assets:" <> name <> ":" <> actId
  Equities "" -> "Assets:" <> name <> ":Equities"
  Equities actId -> "Assets:" <> name <> ":" <> actId <> ":Equities"
  Futures "" -> "Assets:" <> name <> ":Futures"
  Futures actId -> "Assets:" <> name <> ":" <> actId <> ":Futures"
  Options "" -> "Assets:" <> name <> ":Options"
  Options actId -> "Assets:" <> name <> ":" <> actId <> ":Options"
  FuturesOptions "" -> "Assets:" <> name <> ":Futures:Options"
  FuturesOptions actId -> "Assets:" <> name <> ":" <> actId <> ":Futures:Options"
  Forex "" -> "Assets:" <> name <> ":Forex"
  Forex actId -> "Assets:" <> name <> ":" <> actId <> ":Forex"
  Cash "" -> "Assets:" <> name <> ":Cash"
  Cash actId -> "Assets:" <> name <> ":" <> actId <> ":Cash"
  Bonds "" -> "Assets:" <> name <> ":Bonds"
  Bonds actId -> "Assets:" <> name <> ":" <> actId <> ":Bonds"
  MoneyMarkets "" -> "Assets:" <> name <> ":MoneyMarkets"
  MoneyMarkets actId -> "Assets:" <> name <> ":" <> actId <> ":MoneyMarkets"
  Fees -> "Expenses:" <> name <> ":Fees"
  Charges -> "Expenses:" <> name <> ":Charges"
  Commissions -> "Expenses:" <> name <> ":Commission"
  CapitalGainLong -> "Income:Capital:Long"
  CapitalGainShort -> "Income:Capital:Short"
  CapitalGainCollectible -> "Income:Capital:Collectible"
  CapitalLossLong -> "Expenses:Capital:Long"
  CapitalLossShort -> "Expenses:Capital:Short"
  CapitalLossCollectible -> "Expenses:Capital:Collectible"
  CapitalWashLoss -> "Expenses:Capital:Wash"
  CapitalWashLossDeferred -> "Expenses:Capital:Wash:Deferred"
  RoundingError -> "Expenses:" <> name <> ":Rounding"
  OpeningBalances -> "Equity:" <> name <> ":Opening Balances"
  Other txt -> txt
  Unknown -> "Unknown"

renderPosting :: (KnownNat n) => Text -> Posting n -> [Text]
renderPosting name Posting {..} =
  [ T.pack $
      printf
        "    %-32s%16s%s"
        (if _isVirtual then "(" <> act <> ")" else act)
        tip
        (case rest of y : _ -> " " <> y; [] -> "")
    | _amount /= MetadataOnly
  ]
    ++ renderMetadata _postMetadata
  where
    act = renderAccount name _account
    tip :| rest = renderPostingAmount _amount

renderMetadata :: Map Text Text -> [Text]
renderMetadata = Prelude.map go . M.assocs
  where
    go (k, v) = "    ; " <> k <> ": " <> v

renderTransaction :: (KnownNat n) => Text -> Transaction o n -> [Text]
renderTransaction name xact =
  [ T.concat $
      [T.pack (iso8601Show (xact ^. actualDate))]
        ++ maybeToList (T.pack . iso8601Show <$> xact ^. effectiveDate)
        ++ [" * (", xact ^. code, ") ", xact ^. payee]
  ]
    ++ renderMetadata (xact ^. xactMetadata)
    ++ concatMap (renderPosting name) (xact ^. postings)

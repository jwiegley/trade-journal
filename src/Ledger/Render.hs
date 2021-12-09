{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Control.Monad
import Data.Char (isAlpha)
import Data.Coerce
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format.ISO8601
import GHC.TypeLits
import Ledger
import Text.Printf
import Prelude hiding (Double, Float)

renderPostingAmount :: KnownNat n => PostingAmount n -> [Text]
renderPostingAmount MetadataOnly = error "unexpected MetadataOnly"
renderPostingAmount NullAmount = [""]
renderPostingAmount (DollarAmount amt) = ["$" <> T.pack (thousands amt)]
renderPostingAmount (CommodityAmount l@CommodityLot {..}) =
  map
    T.pack
    [ renderAmount _quantity,
      printf
        "%s%s%s%s%s"
        ( if T.all isAlpha _symbol
            then _symbol
            else "\"" <> _symbol <> "\""
        )
        ( maybe
            ""
            (T.pack . printf " {$%s}" . thousands . abs)
            (perShareCost l)
        )
        (maybe "" (T.pack . printf " [%s]" . iso8601Show) _purchaseDate)
        (maybe "" (T.pack . printf " (%s)") _note)
        (maybe "" (T.pack . printf " @ $%s" . thousands) _price)
    ]
  where
    perShareCost CommodityLot {..} =
      fmap coerce ((/ _quantity) <$> _cost) :: Maybe (Amount 6)

renderAccount :: Text -> Account -> Text
renderAccount name = \case
  Equities actId -> "Assets:" <> name <> ":" <> actId <> ":Equities"
  Futures actId -> "Assets:" <> name <> ":" <> actId <> ":Futures"
  Options actId -> "Assets:" <> name <> ":" <> actId <> ":Options"
  FuturesOptions actId -> "Assets:" <> name <> ":" <> actId <> ":Futures:Options"
  Forex actId -> "Assets:" <> name <> ":" <> actId <> ":Forex"
  Cash actId -> "Assets:" <> name <> ":" <> actId <> ":Cash"
  Bonds actId -> "Assets:" <> name <> ":" <> actId <> ":Bonds"
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

renderPosting :: KnownNat n => Text -> Posting n -> [Text]
renderPosting name Posting {..} =
  [ T.pack $
      printf
        "    %-32s%16s%s"
        (if _isVirtual then "(" <> act <> ")" else act)
        (head xs)
        (case xs of _ : y : _ -> " " <> y; _ -> "")
    | _amount /= MetadataOnly
  ]
    ++ renderMetadata _postMetadata
  where
    act = renderAccount name _account
    xs = renderPostingAmount _amount

renderMetadata :: Map Text Text -> [Text]
renderMetadata = Prelude.map go . M.assocs
  where
    go (k, v) = "    ; " <> k <> ": " <> v

renderTransaction :: KnownNat n => Text -> Transaction o n -> [Text]
renderTransaction name xact =
  [ T.concat $
      [T.pack (iso8601Show (xact ^. actualDate))]
        ++ maybeToList (T.pack . iso8601Show <$> xact ^. effectiveDate)
        ++ [" * (", xact ^. code, ") ", xact ^. payee]
  ]
    ++ renderMetadata (xact ^. xactMetadata)
    ++ concatMap (renderPosting name) (xact ^. postings)

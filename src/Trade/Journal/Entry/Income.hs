{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Trade.Journal.Entry.Income where

import Amount
import Control.Applicative
import Control.Lens
import Data.Data
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import GHC.Generics hiding (to)
import Text.Show.Pretty
import Trade.Journal.Print
import Trade.Journal.Types.Entry
import Trade.Journal.Types.Lot
import Prelude hiding (Double, Float)

-- | An Event represents "internal events" that occur within an account,
--   either directly due to the actions above, or indirectly because of other
--   factors.
data Income
  = Dividend !(Amount 2) !Lot -- dividend paid on a long position
  | Interest !(Amount 2) !(Maybe Text) -- interest earned
  | Income !(Amount 2) -- taxable income earned
  | Credit !(Amount 2) -- account credit received
  deriving
    ( Show,
      PrettyVal,
      Eq,
      Generic,
      Data
    )

makePrisms ''Income

_IncomeLot :: Traversal' Income Lot
_IncomeLot f = \case
  Dividend amt lot -> Dividend amt <$> f lot
  Interest amt sym -> pure $ Interest amt sym
  Income amt -> pure $ Income amt
  Credit amt -> pure $ Credit amt

instance HasLot Income where
  _Lot f s = s & _IncomeLot %%~ f

_IncomeNetAmount :: Fold Income (Amount 2)
_IncomeNetAmount f = \case
  Dividend amt _lot -> Income <$> f amt
  Interest amt _sym -> Income <$> f amt
  Income amt -> Income <$> f amt
  Credit amt -> Income <$> f amt

instance HasNetAmount Income where
  _NetAmount f s = s & _IncomeNetAmount %%~ f

printIncome :: Income -> TL.Text
printIncome = \case
  Dividend amt lot -> "dividend " <> printAmount 2 amt <> " " <> printLot lot
  Interest amt Nothing -> "interest " <> printAmount 2 amt
  Interest amt (Just sym) ->
    "interest " <> printAmount 2 amt <> " from " <> printString sym
  Income amt -> "income " <> printAmount 2 amt
  Credit amt -> "credit " <> printAmount 2 amt

instance Printable Income where
  printItem = printIncome

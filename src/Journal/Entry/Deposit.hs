{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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

module Journal.Entry.Deposit where

import Amount
import Control.Applicative
import Control.Lens
import Data.Sum.Lens
import qualified Data.Text.Lazy as TL
import GHC.Generics hiding (to)
import Journal.Parse
import Journal.Print
import Journal.Types.Entry
import Journal.Types.Lot
import Text.Show.Pretty
import Prelude hiding (Double, Float)

-- | An Event represents "internal events" that occur within an account,
--   either directly due to the actions above, or indirectly because of other
--   factors.
data Deposit
  = Deposit (Amount 2) -- deposit (or withdraw if negative)
  | Transfer Lot -- deposits not in the base currency
  deriving
    ( Show,
      PrettyVal,
      Eq,
      Generic
    )

makePrisms ''Deposit

_DepositLot :: Traversal' Deposit Lot
_DepositLot f = \case
  Deposit amt -> pure $ Deposit amt
  Transfer lot -> Transfer <$> f lot

instance HasLot (Const Deposit) where
  _Lot f (Const s) = fmap Const $ s & _DepositLot %%~ f

_DepositNetAmount :: Fold Deposit (Amount 2)
_DepositNetAmount f = \case
  Deposit amt -> Deposit <$> f amt
  Transfer _lot -> Deposit <$> f 0

instance HasNetAmount (Const Deposit) where
  _NetAmount f (Const s) = fmap Const $ s & _DepositNetAmount %%~ f

printDeposit :: Deposit -> TL.Text
printDeposit = \case
  Deposit amt -> "deposit " <> printAmount 2 amt
  Transfer lot -> "transfer " <> printLot lot

instance Printable (Const Deposit) where
  printItem = printDeposit . getConst

parseDeposit :: Parser Deposit
parseDeposit =
  keyword "deposit" *> (Deposit <$> parseAmount)
    <|> keyword "withdraw" *> (Deposit . negate <$> parseAmount)
    <|> keyword "transfer" *> (Transfer <$> parseLot)
    <|> keyword "transfer out " *> (Transfer . over amount negate <$> parseLot)

instance Producible Parser (Const Deposit) where
  produce = fmap Const parseDeposit

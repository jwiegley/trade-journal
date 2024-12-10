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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Trade.Journal.Entry.Deposit where

import Amount
import Control.Applicative
import Control.Lens
import Data.Data
import Data.Text (Text)
import qualified Data.Text as T
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
data Deposit
  = Deposit !(Amount 2) !Text -- deposit (or withdraw if negative)
  | Transfer !Lot !Text -- deposits not in the base currency
  deriving
    ( Show,
      PrettyVal,
      Eq,
      Generic,
      Data
    )

makePrisms ''Deposit

_DepositLot :: Traversal' Deposit Lot
_DepositLot f = \case
  Deposit amt acct -> pure $ Deposit amt acct
  Transfer lot acct -> Transfer <$> f lot <*> pure acct

instance HasLot Deposit where
  _Lot f s = s & _DepositLot %%~ f

_DepositNetAmount :: Fold Deposit (Amount 2)
_DepositNetAmount f = \case
  Deposit amt acct -> Deposit <$> f amt <*> pure acct
  Transfer _lot acct -> Deposit <$> f 0 <*> pure acct

instance HasNetAmount Deposit where
  _NetAmount f s = s & _DepositNetAmount %%~ f

printDeposit :: Deposit -> TL.Text
printDeposit = \case
  Deposit amt acct ->
    ( if amt < 0
        then "withdraw "
        else "deposit "
    )
      <> printAmount 2 amt
      <> if T.null acct
        then ""
        else
          ( if amt < 0
              then " to "
              else " from "
          )
            <> TL.pack (show acct)
  Transfer lot acct ->
    "transfer "
      <> printLot lot
      <> if T.null acct
        then ""
        else
          ( if lot ^. amount < 0
              then " to "
              else " from "
          )
            <> TL.pack (show acct)

instance Printable Deposit where
  printItem = printDeposit

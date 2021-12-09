{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Ledger.Entry where

import Control.Lens
import Data.Sum
import Data.Sum.Lens
import Ledger
import Prelude hiding (Double, Float)

-- | Fold from the given entry type into its corresponding Ledger transaction.
class HasLedgerRepr f where
  _LedgerRepr :: Fold (f v) (Transaction o n)

instance HasFold HasLedgerRepr fs => HasLedgerRepr (Sum fs) where
  _LedgerRepr = plied @HasLedgerRepr _LedgerRepr

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Ledger.Entry where

import Control.Lens
import Ledger
import Prelude hiding (Double, Float)

-- | Fold from the given entry type into its corresponding Ledger transaction.
class HasLedgerRepr f where
  _LedgerRepr :: Fold (f v) (Transaction o n)

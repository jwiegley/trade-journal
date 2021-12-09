{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Journal.Types.Entry where

import Amount
import Control.Lens
import Data.Sum
import Journal.SumLens

-- | The 'netAmount' indicates the exact effect on account balance this action
--   represents.
class HasNetAmount f where
  _NetAmount :: Fold (f v) (Amount 2)

instance HasFold HasNetAmount fs => HasNetAmount (Sum fs) where
  _NetAmount = plied @HasNetAmount _NetAmount

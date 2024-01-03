{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Journal.Types.Entry where

import Amount
import Control.Lens

-- | The 'netAmount' indicates the exact effect on account balance this action
--   represents.
class HasNetAmount a where
  _NetAmount :: Fold a (Amount 2)

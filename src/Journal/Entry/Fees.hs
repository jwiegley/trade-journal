{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Journal.Entry.Fees where

import Amount
import Control.Applicative
import Control.Lens
import Data.Data
import GHC.Generics hiding (to)
import Text.Show.Pretty
import Prelude hiding (Double, Float)

data Fees = Fees
  { _fees :: !(Amount 6),
    _commission :: !(Amount 6)
  }
  deriving (Show, PrettyVal, Eq, Generic, Data)

makeLenses ''Fees

totalFees :: Fold Fees (Amount 6)
totalFees f Fees {..} =
  Fees <$> f _fees <*> f _commission

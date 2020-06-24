module ThinkOrSwim.Types where

import Control.Lens
import Data.Ledger as Ledger
import Data.Time
import Prelude hiding ((<>), Double, Float)

daysApart :: CommodityLot k -> Day -> Maybe Integer
daysApart x yd = do
  xd <- x ^. purchaseDate
  pure $ xd `diffDays` yd

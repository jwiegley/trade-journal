module ThinkOrSwim.Types where

import Control.Lens
import Data.Ledger as Ledger
import Data.Time
import Prelude hiding (Float, Double, (<>))
import ThinkOrSwim.API.TransactionHistory.GetTransactions
           as API hiding ((<+>))

transactionRef :: API.Transaction -> Ref
transactionRef t = Ref OpeningOrder (t^.xid)

daysApart :: CommodityLot k -> Day -> Maybe Integer
daysApart x yd = do
    xd <- x^.purchaseDate
    pure $ xd `diffDays` yd

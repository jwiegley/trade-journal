module ThinkOrSwim.Types where

import Control.Lens
import Data.Ledger as Ledger
import Data.Time
import Prelude hiding (Float, Double, (<>))
import Text.PrettyPrint
import ThinkOrSwim.API.TransactionHistory.GetTransactions
           as API hiding ((<+>))

import Debug.Trace (traceM)

renderM :: Applicative f => Doc -> f ()
renderM = traceM . render

transactionRef :: API.Transaction -> Ref
transactionRef t = Ref OpeningOrder (t^.xactId)

daysApart :: CommodityLot k -> Day -> Maybe Integer
daysApart x yd = do
    xd <- x^.purchaseDate
    pure $ xd `diffDays` yd

-- Return True if 'x' is an open and 'y' is a close.
pairedCommodityLots :: CommodityLot API.TransactionSubType
                    -> CommodityLot API.TransactionSubType
                    -> Bool
pairedCommodityLots x y
    | x' <- x^?Ledger.instrument,
      y' <- y^?Ledger.instrument, x' /= y' = False
    | x^.quantity < 0 && y^.quantity > 0 = True
    | x^.quantity > 0 &&
      (y^.quantity < 0 || y^.kind == OptionExpiration) = True
    | otherwise = False

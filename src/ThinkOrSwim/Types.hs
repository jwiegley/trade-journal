module ThinkOrSwim.Types where

import Control.Applicative ((<|>))
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

transactionRef :: API.Transaction -> Ref API.Transaction
transactionRef t = Ref OpeningOrder (t^.xactId) (Just t)

lotDate :: CommodityLot k API.Transaction -> Maybe Day
lotDate l = l^.purchaseDate
    <|> utctDay <$> l^?refs._head.refOrig._Just.xactDate

daysApart :: CommodityLot k API.Transaction -> Day -> Maybe Integer
daysApart x yd = do
    xd <- lotDate x
    pure $ xd `diffDays` yd

-- Return True if 'x' is an open and 'y' is a close.
pairedCommodityLots :: CommodityLot API.TransactionSubType t
                    -> CommodityLot API.TransactionSubType t
                    -> Bool
pairedCommodityLots x y
    | x' <- x^?Ledger.instrument,
      y' <- y^?Ledger.instrument, x' /= y' = False
    | x^.quantity < 0 && y^.quantity > 0 = True
    | x^.quantity > 0 &&
      (y^.quantity < 0 || y^.kind == OptionExpiration) = True
    | otherwise = False

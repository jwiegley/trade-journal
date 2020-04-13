module ThinkOrSwim.Types where

import Data.Map (Map)
import Data.Ledger
import Data.Text (Text)

type OpenTransactions t = Map Text [CommodityLot t]


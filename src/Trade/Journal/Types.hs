{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Trade.Journal.Types where

import Amount
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time

data TimePrice = TimePrice
  { price :: Amount 2,
    time :: UTCTime
  }
  deriving (Eq, Show)

data Lot = Lot
  { lotAmount :: Amount 2,
    lotDetail :: TimePrice
  }
  deriving (Eq, Show)

data Trade = Trade
  { tradeLot :: Lot,
    tradeFees :: Amount 2
  }
  deriving (Eq, Show)

data Deposit = Deposit (Amount 2)
  deriving (Eq, Show)

data Withdrawal = Withdrawal (Amount 2)
  deriving (Eq, Show)

data Entry
  = TradeEntry
      { tradeAssetFrom :: Text,
        tradeAssetTo :: Text,
        tradeCost :: Maybe (Amount 2),
        tradeEntry :: Trade
      }
  | -- | OptionTradeEntry OptionTrade
    -- | IncomeEntry Income
    DepositEntry
      { depositAsset :: Text,
        depositEntry :: Deposit
      }
  | WithdrawalEntry
      { withdrawalAsset :: Text,
        withdrawalEntry :: Withdrawal
      }
  deriving (Eq, Show)

data OpenPosition = OpenPosition
  { openLot :: Lot,
    openBasis :: Maybe (Amount 2) -- if present, overrides lot price
  }
  deriving (Eq, Show)

data ClosedPosition = ClosedPosition
  { closingLot :: Lot,
    closingDetail :: TimePrice,
    closingWashable :: Bool -- if True, closing can be washed
  }
  deriving (Eq, Show)

-- | Position is basically Either an open or closed position.
data Position
  = Open OpenPosition
  | Closed ClosedPosition
  deriving (Eq, Show)

data PositionChange
  = -- | Position not changed by considered lot.
    PositionUnchanged Position
  | -- | Lot opened an entirely new position.
    PositionOpen Lot
  | -- | Lot resulted in increase the amount of an existing, open position by
    --   the given amount. NOTE: Lot details must match exactly (same basis
    --   price and date), otherwise it represents a new position.
    PositionIncrease OpenPosition (Amount 2)
  | -- | Lot resulted in partially closing an existing, open position. NOTE:
    --   Lot amount must be less than the size of the open position.
    PositionPartialClose OpenPosition Lot
  | -- | Lot resulted in fully closing an existing, open position. Note that
    --   if the size of the lot was larger than the position, there will be an
    --   additional value of 'PositionOpen' in the resulting list. NOTE: Lot
    --   amount must exactly match size of open position.
    PositionClose OpenPosition TimePrice
  deriving (Eq, Show)

validPositionChange :: PositionChange -> Bool
validPositionChange = \case
  PositionUnchanged {} -> True
  PositionOpen {} -> True
  PositionIncrease {} -> True
  PositionPartialClose (OpenPosition x _) y ->
    lotAmount y < lotAmount x
  PositionClose {} -> True

-- This must be an involutive (@f . f = id@) function that reorders
-- transactions according to the order they should be closed in, and undoes
-- that ordering if called again. It is typically either the functions `id` or
-- `reverse`.
type Strategy = [Position] -> [Position]

newtype Journal a = Journal {getJournal :: [(a, Trade)]}
  deriving (Eq, Show)

newJournal :: Journal a
newJournal = Journal []

newtype (Ord a) => Ledger a = Ledger {getLedger :: Map a [Position]}
  deriving (Eq, Show)

newLedger :: (Ord a) => Ledger a
newLedger = Ledger mempty

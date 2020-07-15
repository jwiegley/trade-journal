{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Journal.Types where

import Control.Applicative
import Control.Lens
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Text (Text)
import Data.Time
import Data.Time.Format.ISO8601
import GHC.Generics hiding (to)
import GHC.TypeLits
import Journal.Amount
import Journal.Split
import Text.Show.Pretty
import Prelude hiding (Double, Float)

data Effect = Open | Close
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, PrettyVal)

data Annotation
  = Position Effect
  | Fees (Amount 6)
  | Commission (Amount 6)
  | Gain (Amount 6)
  | Loss (Amount 6)
  | Washed (Amount 6)
  | WashTo (Maybe Text) (Maybe (Amount 6, Amount 6))
  | WashApply Text (Amount 6)
  | Exempt
  | Net (Amount 2)
  | Balance (Amount 2)
  | Account Text
  | Trade Text
  | Order Text
  | Transaction Text
  | Meta Text Text
  deriving (Show, Eq, Ord, Generic, PrettyVal)

makePrisms ''Annotation

-- | The '_Adjustments' traversal returns any adjust to the price of an
-- action. For example, a prior wash sale may increase the cost basis of a
-- position, or a gain might decrease it.
adjustment :: Traversal' Annotation (Amount 6)
adjustment f = \case
  Fees x -> Fees <$> f x
  Commission x -> Commission <$> f x
  Gain x -> Gain . negate <$> f (- x)
  Loss x -> Loss <$> f x
  Washed x -> Washed <$> f x
  x -> x <$ f 0

instance PrettyVal UTCTime where
  prettyVal = String . iso8601Show

-- | A 'Lot' represents a collection of shares, with a given price and a
--   transaction date.
data Lot = Lot
  { _amount :: Amount 6,
    _symbol :: Text,
    _price :: Amount 6,
    -- | All annotations that relate to lot shares are expressed "per share",
    -- just like the price.
    _details :: [Annotation]
  }
  deriving (Show, Eq, Ord, Generic, PrettyVal)

makeLenses ''Lot

alignLots :: Lot -> Lot -> (Split Lot, Split Lot)
alignLots = align amount amount

totaled :: KnownNat n => Lot -> Amount n -> Amount n
totaled lot n = lot ^. amount . coerced * n

fees :: Traversal' Lot (Amount 6)
fees = details . traverse . failing _Fees _Commission

-- | The '_Adjustments' traversal returns any adjust to the price of an
-- action. For example, a prior wash sale may increase the cost basis of a
-- position, or a gain might decrease it.
adjustments :: Traversal' Lot (Amount 6)
adjustments = details . traverse . adjustment

data Action
  = Buy Lot
  | Sell Lot
  | Wash Lot
  | Deposit Lot
  | Withdraw Lot
  | Assign Lot
  | Expire Lot
  | Dividend Lot
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal
    )

makePrisms ''Action

foldAction :: (Lot -> a) -> Action -> a
foldAction f = \case
  Buy lot -> f lot
  Sell lot -> f lot
  Wash lot -> f lot
  Deposit lot -> f lot
  Withdraw lot -> f lot
  Assign lot -> f lot
  Expire lot -> f lot
  Dividend lot -> f lot

mapAction :: (Lot -> Lot) -> Action -> Action
mapAction f = \case
  Buy lot -> Buy (f lot)
  Sell lot -> Sell (f lot)
  Wash lot -> Wash (f lot)
  Deposit lot -> Deposit (f lot)
  Withdraw lot -> Withdraw (f lot)
  Assign lot -> Assign (f lot)
  Expire lot -> Expire (f lot)
  Dividend lot -> Dividend (f lot)

_Lot :: Lens' Action Lot
_Lot f = \case
  Buy lot -> Buy <$> f lot
  Sell lot -> Sell <$> f lot
  Wash lot -> Wash <$> f lot
  Deposit lot -> Deposit <$> f lot
  Withdraw lot -> Withdraw <$> f lot
  Assign lot -> Assign <$> f lot
  Expire lot -> Expire <$> f lot
  Dividend lot -> Dividend <$> f lot

lotNetAmount :: Lot -> Amount 6
lotNetAmount lot = totaled lot (lot ^. price + sum (lot ^.. adjustments))

-- | The 'netAmount' indicates the exact effect on account balance this action
-- represents.
netAmount :: Action -> Amount 2
netAmount = view coerced . \case
  Buy lot -> - totaled lot (lot ^. price + sum (lot ^.. fees))
  Sell lot -> totaled lot (lot ^. price - sum (lot ^.. fees))
  Wash _lot -> 0
  Deposit lot -> lot ^. amount
  Withdraw lot -> - (lot ^. amount)
  Assign _lot -> 0
  Expire _lot -> 0
  Dividend lot -> lot ^. amount

data Event
  = Opened Bool Lot
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal
    )

makePrisms ''Event

data Timed a = Timed
  { _time :: UTCTime,
    _item :: a
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal,
      Functor,
      Foldable,
      Traversable
    )

makeLenses ''Timed

data Change
  = SawAction (Timed Action)
  | Submit (Timed Lot) -- can only be an adjustment
  | SubmitEnd
  | Result (Timed Action)
  | AddEvent (Timed Event)
  | RemoveEvent Int
  | ReplaceEvent Int (Timed Event)
  | SaveWash Text (Timed Lot)
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal
    )

makePrisms ''Change

data InstrumentState = InstrumentState
  { _events :: IntMap (Timed Event),
    _washSales :: Map Text [Timed Lot]
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic
    )

makeLenses ''InstrumentState

newInstrumentState :: InstrumentState
newInstrumentState = InstrumentState mempty mempty

data AccountState = AccountState
  { _nextId :: Int,
    _balance :: Amount 2,
    _instruments :: Map Text InstrumentState
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic
    )

makeLenses ''AccountState

newAccountState :: AccountState
newAccountState = AccountState 1 0 mempty

data JournalState = JournalState
  { _accounts :: Map (Maybe Text) AccountState
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic
    )

makeLenses ''JournalState

newJournalState :: JournalState
newJournalState = JournalState mempty

data Journal = Journal
  { _actions :: [Timed Action]
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal
    )

makeLenses ''Journal

newJournal :: Journal
newJournal = Journal []

data JournalError
  = ChangeNotFromImpliedChanges Change
  | UnexpectedRemainder (Timed Action)
  | UnappliedWashSale (Timed Action)
  | NetAmountDoesNotMatch (Timed Action) (Amount 2) (Amount 2)
  | BalanceDoesNotMatch (Timed Action) (Amount 2) (Amount 2)
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal
    )

-- "Change not produced by impliedChanges"
-- "impliedChanges: unexpected remainder: "
-- "Unapplied wash sale requires use of \"wash to\""

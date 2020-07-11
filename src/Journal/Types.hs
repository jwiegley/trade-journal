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
import GHC.Generics
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
  | Balance (Amount 2)
  | Account Text
  | Trade Text
  | Order Text
  | Transaction Text
  | Meta Text Text
  deriving (Show, Eq, Ord, Generic, PrettyVal)

makePrisms ''Annotation

instance PrettyVal UTCTime where
  prettyVal = String . iso8601Show

-- | A 'Lot' represents a collection of shares, with a given price and a
--   transaction date.
data Lot = Lot
  { _amount :: Amount 6, -- positive is long, negative is short
    _symbol :: Text,
    _price :: Amount 6,
    -- | All details are expressed "per share", just like the price.
    _details :: [Annotation]
  }
  deriving (Show, Eq, Ord, Generic, PrettyVal)

makeLenses ''Lot

alignLots :: Lot -> Lot -> (Split Lot, Split Lot)
alignLots = align amount amount

lotFees :: Lot -> Amount 6
lotFees x = sum (x ^.. details . traverse . failing _Fees _Commission)

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
newAccountState = AccountState 1 mempty

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

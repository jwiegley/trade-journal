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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time
import GHC.Generics
import Journal.Amount
import Journal.Split
import Text.Show.Pretty
import Prelude hiding (Double, Float)

data Effect = Open | Close
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, PrettyVal)

data WashReason = Retroactively | OnOpen
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, PrettyVal)

data Annotation
  = Fees (Amount 6)
  | Commission (Amount 6)
  | Gain (Amount 6)
  | Loss (Amount 6)
  | Washed WashReason (Amount 6)
  | PartWashed
  | Position Effect
  deriving (Show, Eq, Ord, Generic, PrettyVal)

makePrisms ''Annotation

gainLoss :: Traversal' Annotation (Amount 6)
gainLoss f = \case
  Gain pl -> Gain <$> f pl
  Loss pl -> Loss <$> f (negate pl)
  s -> pure s

fees :: [Annotation] -> Amount 6
fees [] = 0
fees (Fees x : xs) = x + fees xs
fees (Commission x : xs) = x + fees xs
fees (_ : xs) = fees xs

instance PrettyVal UTCTime where
  prettyVal = fromMaybe (error "Could not render time") . reify

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

data Action
  = Buy Lot
  | Sell Lot
  | Adjust Lot
  | Deposit (Amount 6)
  | Withdraw (Amount 6)
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

data Event
  = Opened Bool Lot
  | Adjustment Lot
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
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal
    )

makePrisms ''Change

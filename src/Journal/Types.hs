{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Journal.Types where

import Control.Applicative
import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Time
import GHC.Generics
import Journal.Amount
import Journal.Split
import Text.Show.Pretty
import Prelude hiding (Double, Float)

data Effect = Open | Close
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, PrettyVal)

data Annotation
  = Fees (Amount 6)
  | Commission (Amount 6)
  | GainLoss (Amount 6)
  | ToBeWashed
  | WashSaleAdjust (Amount 6)
  | PartsWashed
  | Position Effect
  deriving (Show, Eq, Ord, Generic, PrettyVal)

makePrisms ''Annotation

gainLoss :: Traversal' Annotation (Amount 6)
gainLoss f = \case
  GainLoss pl -> GainLoss <$> f pl
  s -> pure s

washSaleAdjust :: Traversal' Annotation (Amount 6)
washSaleAdjust f = \case
  WashSaleAdjust pl -> WashSaleAdjust <$> f pl
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
data Lot
  = Lot
      { _amount :: Amount 6, -- positive is long, negative is short
        _price :: Amount 6,
        _time :: UTCTime,
        -- | All details are expressed "per share", just like the price.
        _details :: [Annotation]
      }
  deriving (Show, Eq, Ord, Generic, PrettyVal)

makeLenses ''Lot

data Action
  = -- | The sign of the Lot 'amount' indicates buy or sell.
    BuySell Lot
  | Wash Lot
  deriving (Show, Eq, Ord, Generic, PrettyVal)

makePrisms ''Action

data StateChange
  = Action Action
  | Clear
  | Submit Action
  | SubmitEnd
  | Result Lot
  | ConsEvent Lot
  | SnocEvent Lot
  deriving (Show, Eq, Ord, Generic, PrettyVal)

makePrisms ''StateChange

alignLots :: Lot -> Lot -> (Split Lot, Split Lot)
alignLots x y =
  let (s, d) = align amount amount (x & amount %~ abs) (y & amount %~ abs)
   in ( if negX
          then s & _Splits . amount %~ negate
          else s,
        if negY
          then d & _Splits . amount %~ negate
          else d
      )
  where
    negX = x ^. amount < 0
    negY = y ^. amount < 0
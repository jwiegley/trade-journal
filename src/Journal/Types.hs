{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Journal.Types where

import Amount
import Control.Applicative
import Control.Lens
import Data.Sum
import Data.Text (Text)
import Data.Time
import Data.Time.Format.ISO8601
import GHC.Generics hiding (to)
import GHC.TypeLits
import Journal.Split
import Journal.SumLens
import Text.Show.Pretty
import Prelude hiding (Double, Float)

instance PrettyVal UTCTime where
  prettyVal = String . iso8601Show

-- jww (2021-11-26): Some annotations should apply only to actions, others
-- only to events.
data Annotation
  = Fees (Amount 6) -- per share fee
  | Commission (Amount 6) -- per share commission
  | Ident Int
  | Order Text
  | Strategy Text
  | Account Text
  | Note Text
  | Meta Text Text
  deriving (Show, PrettyVal, Eq, Ord, Generic)

makePrisms ''Annotation

data Annotated a = Annotated
  { _item :: a,
    _time :: UTCTime,
    -- | All annotations that relate to lot shares are expressed "per share",
    -- just like the price.
    _details :: [Annotation]
  }
  deriving (Show, PrettyVal, Eq, Generic, Functor, Traversable, Foldable)

makeLenses ''Annotated

-- | A 'Lot' represents a collection of shares, with a given price and a
--   transaction date.
data Lot = Lot
  { _amount :: Amount 6,
    _symbol :: Text,
    _price :: Amount 6
  }
  deriving (Show, PrettyVal, Eq, Ord, Generic)

makeLenses ''Lot

instance Splittable (Amount 6) Lot where
  howmuch = amount

totaled :: KnownNat n => Lot -> Amount n -> Amount n
totaled lot n = lot ^. amount . coerced * n

fees :: Traversal' (Annotated a) (Amount 6)
fees = details . traverse . failing _Fees _Commission

-- | An Event represents "internal events" that occur within an account,
-- either directly due to the actions above, or indirectly because of other
-- factors.
data Entry
  = Deposit (Amount 2) -- deposit money into the account
  | Withdraw (Amount 2) -- withdraw money from the account
  --
  | Buy Lot -- buy securities using money in the account
  | Sell Lot -- sell securities for a loss or gain
  --
  | TransferIn Lot -- buy securities using money in the account
  | TransferOut Lot -- sell securities for a loss or gain
  --
  | Exercise Lot -- exercise a long options position
  | Assign Lot -- assignment of a short options position
  | Expire Lot -- expiration of a short options position
  --
  | Dividend (Amount 2) Lot -- dividend paid on a long position
  | Interest (Amount 2) (Maybe Text) -- interest earned
  --
  | Income (Amount 2) -- taxable income earned
  | Credit (Amount 2) -- account credit received
  deriving
    ( Show,
      PrettyVal,
      Eq,
      Generic
    )

makePrisms ''Entry

buyOrSell :: Traversal' Entry Lot
buyOrSell = failing _Buy _Sell

class HasLot f where
  _Lot :: Traversal' (f v) Lot

instance HasTraversal' HasLot fs => HasLot (Sum fs) where
  _Lot = traversing @HasLot _Lot

_EntryLot :: Traversal' Entry Lot
_EntryLot f = \case
  Deposit amt -> pure $ Deposit amt
  Withdraw amt -> pure $ Withdraw amt
  Buy lot -> Buy <$> f lot
  Sell lot -> Sell <$> f lot
  TransferIn lot -> TransferIn <$> f lot
  TransferOut lot -> TransferOut <$> f lot
  Exercise lot -> Exercise <$> f lot
  Assign lot -> Assign <$> f lot
  Expire lot -> Expire <$> f lot
  Dividend amt lot -> Dividend amt <$> f lot
  Interest amt sym -> pure $ Interest amt sym
  Income amt -> pure $ Income amt
  Credit amt -> pure $ Credit amt

instance HasLot (Const Entry) where
  _Lot f (Const s) = fmap Const $ s & _EntryLot %%~ f

-- | The 'netAmount' indicates the exact effect on account balance this action
-- represents.
netAmount :: Fold (Annotated Entry) (Amount 2)
netAmount f ann = case ann ^. item of
  Deposit amt -> (<$ ann) . Deposit <$> f amt
  Withdraw amt -> (<$ ann) . Deposit <$> f (- amt)
  Buy lot ->
    (<$ ann) . Deposit
      <$> f
        ( - totaled
            lot
            (lot ^. price + sum (ann ^.. fees))
            ^. coerced
        )
  Sell lot ->
    (<$ ann) . Deposit
      <$> f
        ( totaled
            lot
            (lot ^. price - sum (ann ^.. fees))
            ^. coerced
        )
  TransferIn lot ->
    (<$ ann) . Deposit
      <$> f (- totaled lot (lot ^. price + sum (ann ^.. fees)) ^. coerced)
  TransferOut lot ->
    (<$ ann) . Deposit
      <$> f (totaled lot (lot ^. price - sum (ann ^.. fees)) ^. coerced)
  Exercise _lot -> (<$ ann) . Deposit <$> f 0 -- jww (2021-06-12): NYI
  Assign _lot -> (<$ ann) . Deposit <$> f 0 -- jww (2021-06-12): NYI
  Expire _lot -> (<$ ann) . Deposit <$> f 0
  Dividend amt _lot -> (<$ ann) . Deposit <$> f amt
  Interest amt _sym -> (<$ ann) . Deposit <$> f amt
  Income amt -> (<$ ann) . Deposit <$> f amt
  Credit amt -> (<$ ann) . Deposit <$> f amt

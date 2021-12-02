{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Journal.Types where

import Amount
import Control.Applicative
import Control.Lens
import Data.Text (Text)
import Data.Time
import Data.Time.Format.ISO8601
import GHC.Generics hiding (to)
import GHC.TypeLits
import Journal.Split
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

-- | An Action represents "external actions" taken by the holder of the
-- account.
data Action
  = Deposit (Amount 2) -- deposit money into the account
  | Withdraw (Amount 2) -- withdraw money from the account
  | Buy Lot -- buy securities using money in the account
  | Sell Lot -- sell securities for a loss or gain
  | TransferIn Lot -- buy securities using money in the account
  | TransferOut Lot -- sell securities for a loss or gain
  | Exercise Lot -- exercise a long options position
  deriving
    ( Show,
      PrettyVal,
      Eq,
      Generic
    )

makePrisms ''Action

buyOrSell :: Traversal' Action Lot
buyOrSell = failing _Buy _Sell

mapAction :: (Lot -> Lot) -> Action -> Action
mapAction f = \case
  Deposit amt -> Deposit amt
  Withdraw amt -> Withdraw amt
  Buy lot -> Buy (f lot)
  Sell lot -> Sell (f lot)
  TransferIn lot -> TransferIn (f lot)
  TransferOut lot -> TransferOut (f lot)
  Exercise lot -> Exercise (f lot)

_ActionLot :: Traversal' Action Lot
_ActionLot f = \case
  Deposit amt -> pure $ Deposit amt
  Withdraw amt -> pure $ Withdraw amt
  Buy lot -> Buy <$> f lot
  Sell lot -> Sell <$> f lot
  TransferIn lot -> TransferIn <$> f lot
  TransferOut lot -> TransferOut <$> f lot
  Exercise lot -> Exercise <$> f lot

-- | The 'netAmount' indicates the exact effect on account balance this action
-- represents.
actionNetAmount :: Annotated Action -> Amount 2
actionNetAmount ann = case ann ^. item of
  Deposit amt -> amt
  Withdraw amt -> - amt
  Buy lot ->
    - totaled
      lot
      (lot ^. price + sum (ann ^.. fees))
      ^. coerced
  Sell lot ->
    totaled
      lot
      (lot ^. price - sum (ann ^.. fees))
      ^. coerced
  TransferIn lot -> - totaled lot (lot ^. price + sum (ann ^.. fees)) ^. coerced
  TransferOut lot -> totaled lot (lot ^. price - sum (ann ^.. fees)) ^. coerced
  Exercise _lot -> 0 -- jww (2021-06-12): NYI

data Disposition = Long | Short
  deriving (Show, PrettyVal, Eq, Ord, Enum, Bounded, Generic)

data Position a = Position
  { _posIdent :: Int,
    _posLot :: Lot,
    _posDisp :: Disposition,
    _posBasis :: Amount 6,
    _posData :: a
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal
    )

data Closing a = Closing
  { _closingPos :: Position a,
    _closingLot :: Lot,
    _closingData :: a
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal
    )

makeLenses ''Position
makeLenses ''Closing

-- | An Event represents "internal events" that occur within an account,
-- either directly due to the actions above, or indirectly because of other
-- factors.
--
-- jww (2021-11-30): It may need to be Event f, with Open (f Position) in
-- order to support wash sale rule without encoding it here.
data Event a
  = Open (Position a)
  | Close (Closing a)
  | Assign Lot -- assignment of a short options position
  | Expire Lot -- expiration of a short options position
  | Dividend (Amount 2) Lot -- dividend paid on a long position
  | Interest (Amount 2) (Maybe Text) -- interest earned
  | Income (Amount 2) -- taxable income earned
  | Credit (Amount 2) -- account credit received
  deriving
    ( Show,
      PrettyVal,
      Eq,
      Generic
    )

makePrisms ''Event

mapEvent :: (Lot -> Lot) -> Event a -> Event a
mapEvent f = \case
  Open pos -> Open (pos & posLot %~ f)
  Close cl -> Close (cl & closingLot %~ f)
  Assign lot -> Assign (f lot)
  Expire lot -> Expire (f lot)
  Dividend amt lot -> Dividend amt (f lot)
  Interest amt sym -> Interest amt sym
  Income amt -> Income amt
  Credit amt -> Credit amt

_EventLot :: Traversal' (Event a) Lot
_EventLot f = \case
  Open pos -> Open <$> (pos & posLot %%~ f)
  Close cl -> Close <$> (cl & closingLot %%~ f)
  Assign lot -> Assign <$> f lot
  Expire lot -> Expire <$> f lot
  Dividend amt lot -> Dividend amt <$> f lot
  Interest amt sym -> pure $ Interest amt sym
  Income amt -> pure $ Income amt
  Credit amt -> pure $ Credit amt

-- | The 'netAmount' indicates the exact effect on account balance this action
-- represents.
eventNetAmount :: Annotated (Event a) -> Amount 2
eventNetAmount ann = case ann ^. item of
  Open pos
    | pos ^. posDisp == Long ->
      - totaled
        (pos ^. posLot)
        (pos ^. posLot . price + sum (ann ^.. fees))
        ^. coerced
  Open pos
    | pos ^. posDisp == Short ->
      totaled
        (pos ^. posLot)
        (pos ^. posLot . price - sum (ann ^.. fees))
        ^. coerced
  Close cl -> undefined
  Assign _lot -> 0 -- jww (2021-06-12): NYI
  Expire _lot -> 0
  Dividend amt _lot -> amt
  Interest amt _sym -> amt
  Income amt -> amt
  Credit amt -> amt

data Entry a = Action Action | Event (Event a)
  deriving
    ( Show,
      PrettyVal,
      Eq,
      Generic
    )

makePrisms ''Entry

_Lot :: Traversal' (Entry a) Lot
_Lot f = \case
  Action act -> Action <$> (act & _ActionLot %%~ f)
  Event ev -> Event <$> (ev & _EventLot %%~ f)

netAmount :: Annotated (Entry a) -> Amount 2
netAmount ann = case ann ^. item of
  Action act -> actionNetAmount (act <$ ann)
  Event ev -> eventNetAmount (ev <$ ann)

opening :: Traversal' (Annotated (Entry a)) (Position a)
opening = item . _Event . _Open

closing :: Traversal' (Annotated (Entry a)) (Closing a)
closing = item . _Event . _Close

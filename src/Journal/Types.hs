{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
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
  | Washed (Amount 6) -- per share washed amount
  | WashTo Text (Maybe (Amount 6, Amount 6))
  | WashApply Text (Amount 6) -- per share wash applied
  | Exempt
  | Idents [Int]
  | Order Text
  | Strategy Text
  | Account Text
  | Note Text
  | Meta Text Text
  deriving (Show, Eq, Ord, Generic, PrettyVal)

makePrisms ''Annotation

data Annotated a = Annotated
  { _item :: a,
    _time :: UTCTime,
    -- | All annotations that relate to lot shares are expressed "per share",
    -- just like the price.
    _details :: [Annotation]
  }
  deriving (Show, Eq, Ord, Generic, PrettyVal, Functor, Traversable, Foldable)

makeLenses ''Annotated

-- | A 'Lot' represents a collection of shares, with a given price and a
--   transaction date.
data Lot = Lot
  { _amount :: Amount 6,
    _symbol :: Text,
    _price :: Amount 6
  }
  deriving (Show, Eq, Ord, Generic, PrettyVal)

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
      Eq,
      Ord,
      Generic,
      PrettyVal
    )

makePrisms ''Action

mapAction :: (Lot -> Lot) -> Action -> Action
mapAction f = \case
  Deposit amt -> Deposit amt
  Withdraw amt -> Withdraw amt
  Buy lot -> Buy (f lot)
  Sell lot -> Sell (f lot)
  TransferIn lot -> TransferIn (f lot)
  TransferOut lot -> TransferOut (f lot)
  Exercise lot -> Exercise (f lot)

_Lot :: Traversal' Action Lot
_Lot f = \case
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
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, PrettyVal)

data Period = Past | Present | Future
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, PrettyVal)

-- | An Event represents "internal events" that occur within an account,
-- either directly due to the actions above, or indirectly because of other
-- factors.
data Event
  = Open Disposition Lot
  | Close Disposition Lot (Amount 6)
  | Wash Period UTCTime Lot
  | Assign Lot -- assignment of a short options position
  | Expire Lot -- expiration of a short options position
  | Dividend (Amount 2) Lot -- dividend paid on a long position
  | Interest (Amount 2) (Maybe Text) -- interest earned
  | Income (Amount 2) -- taxable income earned
  | Credit (Amount 2) -- account credit received
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal
    )

makePrisms ''Event

mapEvent :: (Lot -> Lot) -> Event -> Event
mapEvent f = \case
  Open disp lot -> Open disp (f lot)
  Close disp lot pl -> Close disp (f lot) pl
  Wash period moment lot -> Wash period moment (f lot)
  Assign lot -> Assign (f lot)
  Expire lot -> Expire (f lot)
  Dividend amt lot -> Dividend amt (f lot)
  Interest amt sym -> Interest amt sym
  Income amt -> Income amt
  Credit amt -> Credit amt

_EventLot :: Traversal' Event Lot
_EventLot f = \case
  Open disp lot -> Open disp <$> f lot
  Close disp lot pl -> Close disp <$> f lot <*> pure pl
  Wash period moment lot -> Wash period moment <$> f lot
  Assign lot -> Assign <$> f lot
  Expire lot -> Expire <$> f lot
  Dividend amt lot -> Dividend amt <$> f lot
  Interest amt sym -> pure $ Interest amt sym
  Income amt -> pure $ Income amt
  Credit amt -> pure $ Credit amt

-- | The 'netAmount' indicates the exact effect on account balance this action
-- represents.
eventNetAmount :: Annotated Event -> Amount 2
eventNetAmount ann = case ann ^. item of
  Open Long lot ->
    - totaled
      lot
      (lot ^. price + sum (ann ^.. fees))
      ^. coerced
  Open Short lot ->
    totaled
      lot
      (lot ^. price - sum (ann ^.. fees))
      ^. coerced
  Close Long lot _pl ->
    totaled
      lot
      (lot ^. price - sum (ann ^.. fees))
      ^. coerced
  Close Short lot _pl ->
    - totaled
      lot
      (lot ^. price + sum (ann ^.. fees))
      ^. coerced
  Wash Past _ _lot -> 0
  Wash Present _ _lot -> 0
  Wash Future _ _lot -> 0
  Assign _lot -> 0 -- jww (2021-06-12): NYI
  Expire _lot -> 0
  Dividend amt _lot -> amt
  Interest amt _sym -> amt
  Income amt -> amt
  Credit amt -> amt

crossAnnotate ::
  Annotated (Either Event Action) -> Either (Annotated Event) (Annotated Action)
crossAnnotate ann = case ann ^. item of
  Left x -> Left (x <$ ann)
  Right x -> Right (x <$ ann)

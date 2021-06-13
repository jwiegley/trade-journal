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

instance PrettyVal UTCTime where
  prettyVal = String . iso8601Show

data Annotation
  = Position Effect
  | Fees (Amount 6) -- per share fee
  | Commission (Amount 6) -- per share commission
  | Gain (Amount 6) -- per share gain
  | Loss (Amount 6) -- per share loss
  | Washed (Amount 6) -- per share washed amount
  | WashTo Text (Maybe (Amount 6, Amount 6))
  | WashApply Text (Amount 6) -- per share wash applied
  | Exempt
  | Order Text
  | Strategy Text
  | Account Text
  | Time UTCTime
  | Note Text
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

data Annotated a = Annotated
  { _item :: a,
    -- | All annotations that relate to lot shares are expressed "per share",
    -- just like the price.
    _details :: [Annotation],
    _computed :: [Annotation]
  }
  deriving (Show, Eq, Ord, Generic, PrettyVal, Functor, Traversable, Foldable)

makeLenses ''Annotated

time :: Traversal' (Annotated a) UTCTime
time = details . traverse . _Time

-- | A 'Lot' represents a collection of shares, with a given price and a
--   transaction date.
data Lot = Lot
  { _amount :: Amount 6,
    _symbol :: Text,
    _price :: Amount 6
  }
  deriving (Show, Eq, Ord, Generic, PrettyVal)

makeLenses ''Lot

alignLots :: Lot -> Lot -> (Split Lot, Split Lot)
alignLots = align amount amount

totaled :: KnownNat n => Lot -> Amount n -> Amount n
totaled lot n = lot ^. amount . coerced * n

fees :: Traversal' (Annotated a) (Amount 6)
fees = details . traverse . failing _Fees _Commission

-- | The '_Adjustments' traversal returns any adjust to the price of an
-- action. For example, a prior wash sale may increase the cost basis of a
-- position, or a gain might decrease it.
adjustments :: Fold (Annotated a) (Amount 6)
adjustments f s =
  error "Never used"
    <$ traverse
      f
      ( s ^.. details . traverse . adjustment
          ++ s ^.. computed . traverse . adjustment
      )

data Action
  = Deposit (Amount 2) -- deposit money into the account
  | Withdraw (Amount 2) -- withdraw money from the account
  | Buy Lot -- buy securities using money in the account
  | Sell Lot -- sell securities for a loss or gain
  | TransferIn Lot -- buy securities using money in the account
  | TransferOut Lot -- sell securities for a loss or gain
  | Wash Lot -- wash a previous losing sale
  | Assign Lot -- assignment of a short options position
  | Expire Lot -- expiration of a short options position
  | Exercise Lot -- exercise a long options position
  | Dividend (Amount 2) Lot -- dividend paid on a long position
  | Interest (Amount 2) -- interest earned
  | Income (Amount 2) -- taxable income earned
  | Credit (Amount 2) -- account credit received
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
  Wash lot -> Wash (f lot)
  Assign lot -> Assign (f lot)
  Exercise lot -> Exercise (f lot)
  Expire lot -> Expire (f lot)
  Dividend amt lot -> Dividend amt (f lot)
  Interest amt -> Interest amt
  Income amt -> Income amt
  Credit amt -> Credit amt

_Lot :: Traversal' Action Lot
_Lot f = \case
  Deposit amt -> pure $ Deposit amt
  Withdraw amt -> pure $ Withdraw amt
  Buy lot -> Buy <$> f lot
  Sell lot -> Sell <$> f lot
  TransferIn lot -> TransferIn <$> f lot
  TransferOut lot -> TransferOut <$> f lot
  Wash lot -> Wash <$> f lot
  Assign lot -> Assign <$> f lot
  Exercise lot -> Exercise <$> f lot
  Expire lot -> Expire <$> f lot
  Dividend amt lot -> Dividend amt <$> f lot
  Interest amt -> pure $ Interest amt
  Income amt -> pure $ Income amt
  Credit amt -> pure $ Credit amt

-- | The 'netAmount' indicates the exact effect on account balance this action
-- represents.
netAmount :: Annotated Action -> Amount 2
netAmount ann = case ann ^. item of
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
  Wash _lot -> 0
  Assign _lot -> 0 -- jww (2021-06-12): NYI
  Exercise _lot -> 0 -- jww (2021-06-12): NYI
  Expire _lot -> 0
  Dividend amt _lot -> amt
  Interest amt -> amt
  Income amt -> amt
  Credit amt -> amt

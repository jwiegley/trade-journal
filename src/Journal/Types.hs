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
  | WashTo Text (Maybe (Amount 6, Amount 6))
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
    _details :: [Annotation],
    _computed :: [Annotation]
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
adjustments :: Fold Lot (Amount 6)
adjustments f s =
  error "Never used"
    <$ traverse
      f
      ( s ^.. details . traverse . adjustment
          ++ s ^.. computed . traverse . adjustment
      )

data Action
  = Deposit (Amount 2) Text -- deposit money into the account
  | Withdraw (Amount 2) Text -- withdraw money from the account
  | Buy Lot -- buy securities using money in the account
  | Sell Lot -- sell securities for a loss or gain
  | TransferIn Lot Text -- buy securities using money in the account
  | TransferOut Lot Text -- sell securities for a loss or gain
  | Wash Lot -- wash a previous losing sale
  | Assign Lot -- assignment of a short options position
  | Expire Lot -- expiration of a short options position
  | Exercise Lot -- exercise a long options position
  | Dividend (Amount 2) Lot -- dividend paid on a long position
  | Interest (Amount 2) Text -- interest earned
  | Income (Amount 2) Text -- taxable income earned
  | Credit (Amount 2) Text -- account credit received
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
  Deposit amt desc -> Deposit amt desc
  Withdraw amt desc -> Withdraw amt desc
  Buy lot -> Buy (f lot)
  Sell lot -> Sell (f lot)
  TransferIn lot desc -> TransferIn (f lot) desc
  TransferOut lot desc -> TransferOut (f lot) desc
  Wash lot -> Wash (f lot)
  Assign lot -> Assign (f lot)
  Exercise lot -> Exercise (f lot)
  Expire lot -> Expire (f lot)
  Dividend amt lot -> Dividend amt (f lot)
  Interest amt desc -> Interest amt desc
  Income amt desc -> Income amt desc
  Credit amt desc -> Credit amt desc

_Lot :: Traversal' Action Lot
_Lot f = \case
  Deposit amt desc -> pure $ Deposit amt desc
  Withdraw amt desc -> pure $ Withdraw amt desc
  Buy lot -> Buy <$> f lot
  Sell lot -> Sell <$> f lot
  TransferIn lot desc -> TransferIn <$> f lot <*> pure desc
  TransferOut lot desc -> TransferOut <$> f lot <*> pure desc
  Wash lot -> Wash <$> f lot
  Assign lot -> Assign <$> f lot
  Exercise lot -> Exercise <$> f lot
  Expire lot -> Expire <$> f lot
  Dividend amt lot -> Dividend amt <$> f lot
  Interest amt desc -> pure $ Interest amt desc
  Income amt desc -> pure $ Income amt desc
  Credit amt desc -> pure $ Credit amt desc

lotNetAmount :: Lot -> Amount 6
lotNetAmount lot = totaled lot (lot ^. price + sum (lot ^.. adjustments))

-- | The 'netAmount' indicates the exact effect on account balance this action
-- represents.
netAmount :: Action -> Amount 2
netAmount = \case
  Deposit amt _desc -> amt
  Withdraw amt _desc -> - amt
  Buy lot -> - totaled lot (lot ^. price + sum (lot ^.. fees)) ^. coerced
  Sell lot -> totaled lot (lot ^. price - sum (lot ^.. fees)) ^. coerced
  TransferIn lot _desc -> - totaled lot (lot ^. price + sum (lot ^.. fees)) ^. coerced
  TransferOut lot _desc -> totaled lot (lot ^. price - sum (lot ^.. fees)) ^. coerced
  Wash _lot -> 0
  Assign _lot -> 0
  Exercise _lot -> 0
  Expire _lot -> 0
  Dividend amt _lot -> amt
  Interest amt _desc -> amt
  Income amt _desc -> amt
  Credit amt _desc -> amt

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

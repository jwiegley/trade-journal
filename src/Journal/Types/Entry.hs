{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Journal.Types.Entry where

import Amount
import Control.Applicative
import Control.Lens
import Data.Text (Text)
import GHC.Generics hiding (to)
import Journal.Types.Annotated
import Journal.Types.Lot
import Text.Show.Pretty
import Prelude hiding (Double, Float)

-- | An Event represents "internal events" that occur within an account,
--   either directly due to the actions above, or indirectly because of other
--   factors.
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

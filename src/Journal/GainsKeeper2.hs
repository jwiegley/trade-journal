{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Journal.GainsKeeper2 where

import Amount
import Control.Applicative
import Control.Exception hiding (handle)
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.IntMap (IntMap)
import Data.List (sortOn, tails)
import Data.Map (Map)
import Data.Text (Text)
import Data.Time
import GHC.Generics hiding (to)
import Journal.Split
import Journal.Types hiding (Action, Buy, Sell, _Buy, _Sell, _Wash)
import Journal.Utils
import Pipes
import Text.Show.Pretty hiding (Time)
import Prelude hiding (Double, Float)

data Washed n a = Unwashed UTCTime a | Washed n a

instance (Num n, Ord n, Splittable n a) => Splittable n (Washed n a) where
  howmuch = howmuch

data Closure n a = Closure UTCTime n a

data Trade n = Buy n | Sell n

makePrisms ''Trade

instance (Num n, Ord n) => Splittable n (Trade n) where
  howmuch f = \case
    Buy n -> Buy <$> f n
    Sell n -> Sell <$> f n

data GainsState n a = GainsState
  { -- | True if the open positions represent short positions.
    _shorted :: Bool,
    -- | All of the currently open positions.
    _openPositions :: [Washed n a],
    -- | Closures which have incurred a loss, but that have not yet been
    -- washed into future open positions.
    _losingClosures :: [Closure n a],
    -- | Closures which have not incurred a loss, but may be adjusted by
    -- future losing closures.
    _pendingClosures :: [Closure n a]
  }

makeLenses ''GainsState

gainsKeeperCore ::
  (Num n, Ord n) =>
  UTCTime ->
  n -> -- number of shares to sell (negative to buy)
  State (GainsState n (Trade n)) [Closure n (Trade n)]
gainsKeeperCore t n = do
  (_matched, (left, amt)) <- gets ((`splitN` abs n) . _openPositions)
  openPositions .= left
  when (amt > 0) $ do
    openPositions <>= [Unwashed t (if n < 0 then Sell amt else Buy amt)]
    shorted .= (n < 0)
  -- pure matched
  pure undefined

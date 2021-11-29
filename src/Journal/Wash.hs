{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Journal.Wash (washSaleRule) where

import Amount
import Control.Arrow (second)
import Control.Lens
import Control.Monad.State
import Data.IntMap (IntMap)
import Data.Maybe (fromJust)
import Data.Time
import GHC.Generics hiding (to)
import Journal.Split
import Journal.Types (Disposition (..), Lot (..))
import Journal.Utils (distance, foldrs)
import Text.Show.Pretty hiding (Time)

class HasTime a where
  time :: Lens' a UTCTime

class HasDisposition a where
  disposition :: Lens' a Disposition

class HasLot a where
  lot :: Lens' a Lot

class HasExempt a where
  exempt :: Lens' a Bool

class HasGains a where
  gains :: Lens' a (Amount 6)

class
  ( HasTime o,
    HasDisposition o,
    HasLot o,
    HasExempt o,
    HasTime c,
    HasDisposition c,
    HasLot c,
    HasGains c
  ) =>
  EventLike o c a
    | a -> o,
      a -> c
  where
  _Open :: Prism' a o
  _Close :: Prism' a c

data Washing
  = Exempted
  | Eligible
  | NotApplicable
  | WashFromPast (Amount 6)
  | WashFromFuture (Amount 6)
  | WashedBackward
  | WashedFoward
  deriving
    ( Show,
      PrettyVal,
      Eq,
      Ord,
      Generic
    )

makePrisms ''Washing

-- This implementation of the wash sale rule requires that we know the total
-- set of broker events in advance. Basically, each time we encounter a
-- non-exempt position open, we check 30 days back and then 30 days forward
-- for an eligible losing close; if it exists, the loss is moved into the cost
-- basis of the open.
washSaleRule :: (EventLike o c a, HasExempt a) => [a] -> [(Washing, a)]
washSaleRule =
  go . map (\x -> (if x ^. exempt then Exempted else Eligible, x))
  where
    go :: [(Washing, a)] -> [(Washing, a)]
    go = foldrs (\x xs b -> []) []

{-
washStep ::
  forall a o c.
  EventLike o c a =>
  Maybe a ->
  State (WashState a) [(Washing, a)]
washStep Nothing = gets (^.. events . traverse)
washStep (Just x@(preview _Open -> Just (o :: o)))
  | o ^. exempt = pure [(Exempted, x)]
  | otherwise = do
    -- Since openings and closing arrive in time order, purge any opening
    -- beyond 30 days from this closing.
    cis <- keepLessThanEqDays o 30 =<< use unappliedCloses
    case cis of
      [] -> do
        unappliedCloses .= []
        oi <- use nextId
        nextId += 1
        [] <$ (unwashedOpens %= (oi :))
      ci : cis -> do
        unappliedCloses .= cis
        c <- gets (^?! events . ix ci . _2)
        (: []) . second (\l -> _Open # (o & lot .~ l))
          <$> wash @a WashFromPast c o
washStep (Just (preview _Close -> Just (c :: c))) = do
  ois <- keepLessThanEqDays c 30 =<< use unwashedOpens
  case ois of
    [] -> do
      unwashedOpens .= []
      ci <- use nextId
      nextId += 1
      [] <$ (unappliedCloses %= (ci :))
    oi : ois -> do
      unwashedOpens .= ois
      o <- gets (^?! events . ix oi . _2)
      (: []) . second (\l -> _Close # (c & lot .~ l))
        <$> wash @a WashFromFuture c o
washStep (Just x) = pure [(NotApplicable, x)]

keepLessThanEqDays ::
  (HasTime a, HasTime b) =>
  b ->
  Integer ->
  [Int] ->
  State (WashState a) [Int]
keepLessThanEqDays x d =
  filterM $ \yi -> do
    y <- gets (^?! events . ix yi)
    pure $ abs ((x ^. time) `distance` (y ^. _2 . time)) <= d

wash ::
  forall a o c.
  EventLike o c a =>
  (Amount 6 -> Washing) ->
  c ->
  o ->
  State (WashState a) (Washing, Lot)
wash f c o = do
  fromJust . fst
    <$> alignedA
      (c ^. lot)
      (o ^. lot)
      (\_cu ou -> pure (f (c ^. gains), ou))
      (\ck -> unappliedCloses %= ((c & lot .~ ck) :))
      (\ok -> unwashedOpens %= ((o & lot .~ ok) :))

data Opening = Opening
  { _openTime :: UTCTime,
    _openDisp :: Disposition,
    _openLot :: Lot,
    _openExempt :: Bool
  }
  deriving
    ( Show,
      PrettyVal,
      Eq,
      Ord,
      Generic
    )

makeLenses ''Opening

instance HasTime Opening where
  time = openTime

instance HasDisposition Opening where
  disposition = openDisp

instance HasLot Opening where
  lot = openLot

instance HasExempt Opening where
  exempt = openExempt

data Closing = Closing
  { _closeTime :: UTCTime,
    _closeDisp :: Disposition,
    _closeLot :: Lot,
    _closeGains :: Amount 6
  }
  deriving
    ( Show,
      PrettyVal,
      Eq,
      Ord,
      Generic
    )

makeLenses ''Closing

instance HasTime Closing where
  time = closeTime

instance HasDisposition Closing where
  disposition = closeDisp

instance HasLot Closing where
  lot = closeLot

instance HasGains Closing where
  gains = closeGains

data Event
  = EOpen Opening
  | EClose Closing
  deriving
    ( Show,
      PrettyVal,
      Eq,
      Ord,
      Generic
    )

makePrisms ''Event

instance EventLike Opening Closing Event where
  _Open = _EOpen
  _Close = _EClose

testWashSale :: IO ()
testWashSale = do
  now <- getCurrentTime
  putStrLn $ dumpStr $ washSaleRule (dataset now)
  where
    dataset :: UTCTime -> [Event]
    dataset now =
      [ EOpen
          ( Opening
              now
              Long
              Lot
                { _amount = 100,
                  _symbol = "FOO",
                  _price = 100
                }
              False
          ),
        EOpen
          ( Opening
              now
              Long
              Lot
                { _amount = 200,
                  _symbol = "FOO",
                  _price = 200
                }
              False
          ),
        EOpen
          ( Opening
              now
              Long
              Lot
                { _amount = 300,
                  _symbol = "FOO",
                  _price = 300
                }
              False
          ),
        EClose
          ( Closing
              now
              Long
              Lot
                { _amount = 300,
                  _symbol = "FOO",
                  _price = 300
                }
              (-50)
          ),
        EOpen
          ( Opening
              now
              Long
              Lot
                { _amount = 200,
                  _symbol = "FOO",
                  _price = 200
                }
              False
          ),
        EOpen
          ( Opening
              now
              Long
              Lot
                { _amount = 300,
                  _symbol = "FOO",
                  _price = 300
                }
              False
          ),
        EClose
          ( Closing
              now
              Long
              Lot
                { _amount = 300,
                  _symbol = "FOO",
                  _price = 300
                }
              (-50)
          )
      ]
-}

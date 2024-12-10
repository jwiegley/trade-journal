{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Trade.Journal.Process where

import Amount
import Data.Map qualified as M
import Data.Time
import Trade.Journal.Types
import Trade.Journal.Zipper

data LotChange
  = NoChange
  | AddLot Lot
  | ReduceLot Lot
  | ReplaceLot Lot
  deriving (Eq, Show)

addLot :: Lot -> Lot -> LotChange
addLot (Lot xn xd) (Lot yn yd)
  | xn > 0 && yn < 0 || xn < 0 && yn > 0 =
      if abs xn >= abs yn
        then ReduceLot (Lot (xn + yn) xd)
        else ReplaceLot (Lot (xn + yn) yd)
  | xd == yd = AddLot (Lot (xn + yn) xd)
  | otherwise = NoChange

addToPositions :: Lot -> [Position] -> [Position]
addToPositions = go
  where
    go y [] = [Open y Nothing]
    go y@(Lot _yn yd) (Open z@(Lot zn _zd) wash : zs) =
      case z `addLot` y of
        NoChange ->
          Open z wash : go y zs
        AddLot w ->
          Open w wash : zs
        ReduceLot z'@(Lot zn' zd') ->
          ( if zn' == 0
              then id
              else (Open z' wash :)
          )
            $ Closed (Lot (zn - zn') zd') yd True : zs
        ReplaceLot w ->
          Closed z yd True : go w zs
    go y (c : zs) = c : go y zs

addManyToPositions :: [Lot] -> [Position] -> [Position]
addManyToPositions = flip (foldl' (flip addToPositions))

-- | A wash sale happens when you sell a security at a loss and buy a
--   “substantially identical” security within 30 days before or after the
--   sale.
--
--   The three basic scenarios are as follows, where opening B and closing A
--   are within 30 days of each other:
--
--     Open A --> Open B --> Close A at loss
--                ^--------- WASH
--
--     Open B --> Open A --> Close A at loss
--     ^-------------------- WASH
--
--     Open A --> Close A at loss --> Open B
--                WASH ---------------^
--
--   In both cases, the position A is no longer open, the closing of A is
--   considered "washed", and the opening of B has had its cost basis
--   adjusted.
washSales :: [Position] -> [Position]
washSales = survey go
  where
    go
      ( MkZipper
          before
          event@( Closed
                    l@(Lot n (TimePrice lp ld))
                    (TimePrice cp cd)
                    True
                  )
          after
        )
        | eligibleLosingClose event =
            case break (eligibleNewOpen ld) before of
              (xpre, Open x Nothing : xpost) ->
                MkZipper
                  (xpre ++ adjusted x : xpost)
                  (Closed l (TimePrice lp cd) False)
                  after
              _ -> case break (eligibleNewOpen ld) after of
                (ypre, Open y Nothing : ypost) ->
                  MkZipper
                    before
                    (Closed l (TimePrice lp cd) False)
                    (ypre ++ adjusted y : ypost)
                _ -> justClosed
        | otherwise = justClosed
        where
          totalLoss = n * (lp - cp)
          justClosed = MkZipper before (Closed l (TimePrice cp cd) False) after
          adjusted x@(Lot m (TimePrice o _)) =
            Open x (Just (o + totalLoss / m))
    go z = z

    eligibleLosingClose
      (Closed (Lot n (TimePrice cp _)) (TimePrice cp' _) True) =
        n * (cp' - cp) < 0
    eligibleLosingClose _ = False

    eligibleNewOpen ld (Open (Lot _ (TimePrice _ ld')) Nothing) =
      ld /= ld' && withinDays 30 ld ld'
    eligibleNewOpen _ _ = False

withinDays :: Integer -> UTCTime -> UTCTime -> Bool
withinDays days x y = x `diffUTCTime` y < fromIntegral days * 86400

gainLoss :: Position -> Maybe (Amount 2)
gainLoss
  ( Closed
      (Lot n (TimePrice basis _openTime))
      (TimePrice sale _closeTime)
      _
    ) =
    Just (n * (sale - basis))
gainLoss _ = Nothing

processJournal :: (Ord a) => Ledger a -> Journal a -> Ledger a
processJournal (Ledger poss) (Journal lots) =
  Ledger (foldl' identifyTrade poss lots)
  where
    identifyTrade m (sym, lot) = M.alter (Just . go) sym m
      where
        go Nothing = [Open lot Nothing]
        go (Just ps) = addToPositions lot ps

processLedger ::
  (Ord a) =>
  (a -> [Position] -> [Position]) ->
  Ledger a ->
  Ledger a
processLedger f (Ledger poss) = Ledger (M.mapWithKey f poss)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Trade.Journal.Process where

import Amount
import Data.Map qualified as M
import Data.Maybe (isNothing)
import Data.Time
import Trade.Journal.Types
import Trade.Journal.Zipper

data LotChange
  = NoChange
  | AddLot
  | CloseLot
  | ReduceLot
  | ReplaceLot (Amount 2)
  deriving (Eq, Show)

addLot :: Lot -> Lot -> LotChange
addLot (Lot xn xd) (Lot yn yd)
  | xn > 0 && yn < 0 || xn < 0 && yn > 0 =
      let diff = abs xn - abs yn
       in if diff == 0
            then CloseLot
            else
              if diff > 0
                then ReduceLot
                else ReplaceLot (xn + yn)
  | xd == yd = AddLot
  | otherwise = NoChange

data PositionChange
  = PositionUnchanged Position
  | PositionOpen Lot
  | PositionIncrease OpenPosition (Amount 2)
  | -- | @lotAmount lot < lotAmount (openLot pos)
    PositionPartialClose OpenPosition Lot
  | PositionClose OpenPosition TimePrice
  deriving (Eq, Show)

applyLot :: Lot -> [Position] -> [PositionChange]
applyLot = go
  where
    go y [] = [PositionOpen y]
    go y@(Lot yn yd) (Open o@(OpenPosition z _) : zs) =
      case z `addLot` y of
        NoChange -> PositionUnchanged (Open o) : go y zs
        AddLot -> PositionIncrease o yn : remainder
        CloseLot -> PositionClose o yd : remainder
        ReduceLot -> PositionPartialClose o y : remainder
        ReplaceLot n -> PositionClose o yd : go (Lot n yd) zs
      where
        remainder = map PositionUnchanged zs
    go y (c : zs) = PositionUnchanged c : go y zs

changedPositions :: [PositionChange] -> [Position]
changedPositions [] = []
changedPositions (x : xs) = case x of
  PositionUnchanged p -> p : ys
  PositionOpen l -> Open (OpenPosition l Nothing) : ys
  PositionIncrease (OpenPosition (Lot n tpo) wash) m ->
    Open (OpenPosition (Lot (n + m) tpo) wash) : ys
  PositionPartialClose (OpenPosition (Lot n tpo) wash) (Lot m tpc)
    | m < n ->
        Closed (ClosedPosition (Lot m tpo) tpc (isNothing wash))
          : Open (OpenPosition (Lot (n - m) tpo) wash)
          : ys
    | otherwise ->
        error "Partially closing position with too large an amount"
  PositionClose (OpenPosition n wash) tpc ->
    Closed (ClosedPosition n tpc (isNothing wash)) : ys
  where
    ys = changedPositions xs

addToPositions :: Lot -> [Position] -> [Position]
addToPositions = (changedPositions .) . applyLot

{-
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
-}

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
                    ( ClosedPosition
                        l@(Lot n (TimePrice lp ld))
                        (TimePrice cp cd)
                        True
                      )
                  )
          after
        )
        | eligibleLosingClose event =
            case break (eligibleNewOpen ld) before of
              (xpre, Open (OpenPosition x Nothing) : xpost) ->
                MkZipper
                  (xpre ++ adjusted x : xpost)
                  (Closed (ClosedPosition l (TimePrice lp cd) False))
                  after
              _ -> case break (eligibleNewOpen ld) after of
                (ypre, Open (OpenPosition y Nothing) : ypost) ->
                  MkZipper
                    before
                    (Closed (ClosedPosition l (TimePrice lp cd) False))
                    (ypre ++ adjusted y : ypost)
                _ -> justClosed
        | otherwise = justClosed
        where
          totalLoss = n * (lp - cp)
          justClosed =
            MkZipper
              before
              (Closed (ClosedPosition l (TimePrice cp cd) False))
              after
          adjusted x@(Lot m (TimePrice o _)) =
            Open (OpenPosition x (Just (o + totalLoss / m)))
    go z = z

    eligibleLosingClose
      ( Closed
          ( ClosedPosition
              (Lot n (TimePrice cp _))
              (TimePrice cp' _)
              True
            )
        ) =
        n * (cp' - cp) < 0
    eligibleLosingClose _ = False

    eligibleNewOpen ld (Open (OpenPosition (Lot _ (TimePrice _ ld')) Nothing)) =
      ld /= ld' && withinDays 30 ld ld'
    eligibleNewOpen _ _ = False

withinDays :: Integer -> UTCTime -> UTCTime -> Bool
withinDays days x y = x `diffUTCTime` y < fromIntegral days * 86400

gainLoss :: Position -> Maybe (Amount 2)
gainLoss
  ( Closed
      ( ClosedPosition
          (Lot n (TimePrice basis _openTime))
          (TimePrice sale _closeTime)
          _
        )
    ) =
    Just (n * (sale - basis))
gainLoss _ = Nothing

processJournal :: (Ord a) => Ledger a -> Journal a -> Ledger a
processJournal (Ledger poss) (Journal lots) =
  Ledger (foldl' identifyTrade poss lots)
  where
    identifyTrade m (sym, lot) = M.alter (Just . go) sym m
      where
        go Nothing = [Open (OpenPosition lot Nothing)]
        go (Just ps) = addToPositions lot ps

processLedger ::
  (Ord a) =>
  (a -> [Position] -> [Position]) ->
  Ledger a ->
  Ledger a
processLedger f (Ledger poss) = Ledger (M.mapWithKey f poss)

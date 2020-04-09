{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module ThinkOrSwim.Gains where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Fixed
import Data.Foldable (foldl')
import Data.Ledger as Ledger
import Data.Maybe (fromMaybe)
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import ThinkOrSwim.Types

import Test.Tasty
import Test.Tasty.HUnit

-- import Debug.Trace

-- The function replicates the logic used by GainsKeeper to determine what
-- impact a given transaction, based on existing positions, should have on an
-- account.
gainsKeeper :: API.Transaction -> CommodityLot
            -> State OpenTransactions [(Fixed E6, CommodityLot)]
gainsKeeper t lot = do
    let sym   = lot^.Ledger.symbol
        fees' = t^.fees_.regFee + t^.fees_.otherCharges + t^.fees_.commission
        cst   = abs (t^.item.API.cost - fromRational (toRational fees'))

    use (at sym) >>= \case
        -- If there are no existing lots, then this is either a purchase or a
        -- short sale.
        Nothing -> do
            let l = lot' cst
            let keep = if l^.quantity /= 0 then [l] else []
            at sym .= case keep of [] -> Nothing; xs -> Just xs
            pure $ (0,) <$> keep

        -- If there are existing lots for this symbol, then if the current
        -- would add to or deduct from those positions, then it closes as much
        -- of that previous positions as quantities dictate.
        Just ls -> do
            -- traceM (unpack sym ++ ": calculateGains ("
            --         ++ showCommodityLot (lot' cst)
            --         ++ ") ["
            --         ++ intercalate "," (map showCommodityLot ls)
            --         ++ "]")
            let (res, keep) = calculateGains (lot' cst) ls
            at sym .= case keep of [] -> Nothing; xs -> Just xs
            pure res
  where
    lot' cst = lot
        & Ledger.cost  .~ (if cst /= 0 then Just cst else Nothing)
        & purchaseDate ?~ t^.xactDate
        & refs         .~ [Ref OpeningOrder (t^.xactId)]

(@@) :: Fixed E6 -> Fixed E6 -> CommodityLot
q @@ c = newCommodityLot
    & quantity .~ q
    & Ledger.cost ?~ c

-- Given some lot x, apply lot y. If x is positive, and y is negative, this is
-- a share sell; a buy for the reverse. If both have the same sign, nothing is
-- done. If the cost basis per share of the two are different, there will be a
-- gain (or less, if negative). Also, we need to return the part of 'x' that
-- remains to be further deducted from, and how much was consumed, and
-- similarly for 'y'.
applyLots :: CommodityLot -> CommodityLot
             -> ( Fixed E6
               , ( Maybe CommodityLot -- the portion subtracted out
                 , Maybe CommodityLot -- what remains
                 )
               , Maybe CommodityLot   -- the portion unsubtracted
               )
applyLots x y
    |   x^.quantity < 0 && y^.quantity < 0
      || x^.quantity > 0 && y^.quantity > 0 =
    (0.0, (Nothing, Just x), Just y)
applyLots x y =
    -- trace ("x^.symbol   = " ++ show (x^.Ledger.symbol)) $
    -- trace ("x^.quantity = " ++ show (x^.quantity)) $
    -- trace ("x^.refs     = " ++ show (x^.refs)) $
    -- trace ("y^.quantity = " ++ show (y^.quantity)) $
    -- trace ("y^.refs     = " ++ show (y^.refs)) $
    let xcst = fromMaybe 0.0
            (x^.Ledger.cost <|> ((x^.quantity) *) <$> x^.Ledger.price)
        ycst = fromMaybe 0.0
            (y^.Ledger.cost <|> ((y^.quantity) *) <$> y^.Ledger.price)
        -- xps  = fromMaybe (xcst / x^.quantity) (x^.Ledger.price)
        xps  = xcst / x^.quantity
        -- yps  = fromMaybe (ycst / y^.quantity) (y^.Ledger.price)
        yps  = ycst / y^.quantity
        xq   = abs (x^.quantity)
        yq   = abs (y^.quantity)
        n    = min xq yq
        xn   = if x^.quantity < 0 then (-n) else n
        yn   = if y^.quantity < 0 then (-n) else n
    in -- trace ("xcst = " ++ show xcst) $
       -- trace ("ycst = " ++ show ycst) $
       -- trace ("xps  = " ++ show xps) $
       -- trace ("yps  = " ++ show yps) $
       -- trace ("xq   = " ++ show xq) $
       -- trace ("yq   = " ++ show yq) $
       -- trace ("n    = " ++ show n) $
       -- trace ("gain = " ++ show ((- n * (yps + xps)))) $
       ( (- n * (yps + xps))
       , ( Just $ x & quantity     .~ (- xn)
                    & Ledger.cost  ?~ n * abs xps
                    & Ledger.price .~ y^.Ledger.price
         , if n < xq
           then Just $ x & quantity    -~ xn
                         & Ledger.cost ?~ (xq - n) * abs xps
           else Nothing)
       , if n < yq
         then Just $ y & quantity    -~ yn
                       & Ledger.cost ?~ (yq - n) * abs yps
         else Nothing)

testApplyLots :: TestTree
testApplyLots = testGroup "Gains"
    [ testCase "12@@300 `applyLots` -10@@500" $
      (12@@300) `applyLots` ((-10)@@500)
          @?= ( 10.0 * ((500 / 10) - (300 / 12))
              , ( Just ((-10)@@(10 * (300/12)))
                , Just (2@@(2 * (300/12))))
              , Nothing)

    , testCase "10@@500 `applyLots` -12@@300" $
      (10@@500) `applyLots` ((-12)@@300)
          @?= ( 10.0 * ((300 / 12) - (500 / 10))
              , ( Just ((-10)@@(10 * (500/10)))
                , Nothing)
              , Just ((-2)@@(2 * (300/12))))

    , testCase "12@@500 `applyLots` -10@@300" $
      (12@@500) `applyLots` ((-10)@@300)
          @?= ( 10.0 * ((300 / 10) - (500 / 12))
              , ( Just ((-10)@@(10 * (500/12)))
                , Just (2@@(2 * (500/12))))
              , Nothing)

    , testCase "10@@300 `applyLots` -12@@500" $
      (10@@300) `applyLots` ((-12)@@500)
          @?= ( 116.66666667
              , ( Just ((-10)@@(10 * (300/10)))
                , Nothing)
              , Just ((-2)@@83.333333334))

    , testCase "-10@@300 `applyLots` 12@@500" $
      ((-10)@@300) `applyLots` (12@@500)
          @?= ( -116.66666666
              , ( Just (10@@(10 * (300/10)))
                , Nothing)
              , Just (2@@(2 * (500/12))))

    , testCase "-10@@500 `applyLots` 12@@300" $
      ((-10)@@500) `applyLots` (12@@300)
          @?= ( (-10.0) * ((300 / 12) - (500 / 10)) -- 250.0
              , ( Just (10@@(10 * (500/10)))
                , Nothing)
              , Just (2@@(2 * (300/12))))

    , testCase "calculateGains -400" $
      calculateGains
          ((-400) @@ 69727.28) -- {174.3182}
          [ 100 @@ 17350.00    -- {173.50}
          , 400 @@ 69722.60    -- {174.3065}
          ]
          @?= ( [ (81.82, (-100) @@ 17350.00)
                , (3.51, (-300) @@ 52291.95) ]
              , [100 @@ 17430.65] )

    , testCase "calculateGains 400" $
      calculateGains
          (400 @@ 69722.60)
          [ 100 @@ 17350.00
          ]
          @?= ( [ (0.0, 400 @@ 69722.60) ]
              , [ 100 @@ 17350.00
                , 400 @@ 69722.60
                ] )
    ]

fifo :: (Maybe CommodityLot, [(Fixed E6, CommodityLot)], [CommodityLot])
     -> CommodityLot
     -> (Maybe CommodityLot, [(Fixed E6, CommodityLot)], [CommodityLot])
fifo x@(Nothing, _, _) _ = x
fifo (Just l, res, keep) x =
    let (gain, (used, kept), left) = x `applyLots` l
    in (left, maybe res ((:res) . (gain,)) used, maybe keep (:keep) kept)

calculateGains :: CommodityLot -> [CommodityLot]
               -> ([(Fixed E6, CommodityLot)], [CommodityLot])
calculateGains l ls =
    let (x, xs, ys) = foldl' fifo (Just l, [], []) ls
    in (maybe (reverse xs) (reverse . (:xs) . (0,)) x,
        maybe (reverse ys) (reverse . (:ys)) x)

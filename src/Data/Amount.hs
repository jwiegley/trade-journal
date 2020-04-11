{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Amount where

import           Data.Aeson
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.TypeLits
import           Numeric.Rounded
import           Prelude hiding (Float, Double)
import qualified Prelude
import           Text.Printf

newtype Amount (dec :: Nat) = Amount { getAmount :: Prelude.Double }
    deriving (Show, Read, Eq, Ord, Num,
              Floating, Fractional, Real, RealFloat, RealFrac,
              ToJSON, FromJSON, PrintfArg)

unroundFrom :: forall n m. (KnownNat n, KnownNat m, n <= m)
            => Amount n -> Amount m
unroundFrom = Amount . getAmount

roundTo :: forall n m. (KnownNat n, KnownNat m, m <= n)
        => Amount n -> Amount m
-- roundTo n x = fromIntegral (round (x * (10^n)) :: Int) / 10^n
roundTo = Amount . go . getAmount
  where
    go x =
        let n = natVal (Proxy :: Proxy m) in
        toDouble
         (rint_round_
          (fromDouble x * (10^n) :: Rounded 'TowardNearest 128)
            / 10^n :: Rounded 'TowardNearest 128)

doubleToText :: forall n. KnownNat n => Amount n -> Text
doubleToText =
    let n = natVal (Proxy :: Proxy n) in
    cleanup 2 . T.pack . printf ("%0." ++ show n ++ "f") . roundTo @n @n
  where
    cleanup m t =
        let len = T.length (last (T.splitOn "." t)) in
        if len > m && T.last t == '0'
        then cleanup m (T.take (T.length t - 1) t)
        else t

thousands :: forall n. KnownNat n => Amount n -> Text
thousands d = T.intercalate "." $
    case T.splitOn "." str of
        x:xs -> (T.pack . reverse . go . reverse . T.unpack) x :
            case xs of
                y:ys -> (T.pack . expand . T.unpack) y : ys
                _ | isInt -> ["00"]
                _ -> []
        xs -> xs
  where
    isInt = case natVal (Proxy :: Proxy n) of
        0 -> True
        _ -> False

    str | isInt     = T.pack (show (floor d :: Int))
        | otherwise = doubleToText d

    go (x:y:z:[])    = x:y:z:[]
    go (x:y:z:['-']) = x:y:z:['-']
    go (x:y:z:xs)    = x:y:z:',':go xs
    go xs            = xs

    expand []     = "00"
    expand (x:[]) = x:"0"
    expand xs     = xs

renderDouble :: forall n. KnownNat n => Amount n -> Text
renderDouble d | fromIntegral (floor d :: Int) == d =
    thousands @0 (roundTo d)
renderDouble d = thousands d

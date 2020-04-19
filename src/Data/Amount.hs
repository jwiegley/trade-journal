{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Amount
    ( Amount(..)
    , _Amount
    , rounded
    , thousands
    , renderAmount
    , normalizeAmount
    , spreadAmounts
    , showAmount
    , mpfr_RNDN
    , mpfr_RNDZ
    , mpfr_RNDU
    , mpfr_RNDD
    , mpfr_RNDA
    , mpfr_RNDF
    , mpfr_RNDNA
    ) where

import Data.Aeson
import Data.Char (isDigit)
import Data.Coerce
import Data.Function (on)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.List.Split
import Data.Profunctor
import Data.Proxy
import Data.Ratio
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.TypeLits
import Prelude hiding (Float, Double)
import System.IO.Unsafe

mpfr_RNDN, mpfr_RNDZ, mpfr_RNDU, mpfr_RNDD, mpfr_RNDA, mpfr_RNDF :: CUInt
mpfr_RNDNA :: CUInt

mpfr_RNDN = 0       -- round to nearest, with ties to even
mpfr_RNDZ = 1       -- round toward zero
mpfr_RNDU = 2       -- round toward +Inf
mpfr_RNDD = 3       -- round toward -Inf
mpfr_RNDA = 4       -- round away from zero
mpfr_RNDF = 5       -- faithful rounding
mpfr_RNDNA = 6      -- round to nearest, with ties away from zero (mpfr_round)

foreign import ccall unsafe "mpfr_free_str" c'mpfr_free_str :: CString -> IO ()
foreign import ccall unsafe "rational_to_str" c'rational_to_str
    :: CLong -> CULong -> CUInt -> CSize -> Ptr CString -> IO ()

newtype Amount (dec :: Nat) = Amount { getAmount :: Ratio Int64 }
    deriving (Ord, Num, Fractional, Real, RealFrac)

showAmount :: forall n. KnownNat n => CUInt -> Amount n -> String
showAmount rnd (Amount r) =
    unsafePerformIO $ alloca $ \bufPtr -> do
        c'rational_to_str
            (CLong (numerator r))
            (CULong (fromIntegral (denominator r)))
            rnd
            (CSize (fromIntegral (natVal (Proxy :: Proxy n))))
            bufPtr
        buf <- peek bufPtr
        str <- peekCString buf
        c'mpfr_free_str buf
        return str

instance forall n. KnownNat n => Eq (Amount n) where
    (==) = (==) `on` show

instance forall n. KnownNat n => Show (Amount n) where
    show = amountToString

instance forall n. KnownNat n => Read (Amount n) where
    readsPrec _d = \case
        '-':xs -> map (\(x, y) -> (negate x, y)) (readNum xs)
        xs     -> readNum xs
      where
        readNum r = case takeWhile isDigit r of
            [] -> error $ "Not an amount: " ++ r
            num -> case dropWhile isDigit r of
                ('.':den) ->
                    let den' = takeWhile isDigit den
                        rem' = dropWhile isDigit den
                    in [(Amount (read (num ++ den') % 10 ^ length den'), rem')]
                xs -> [(Amount (read num % 1), xs)]

instance forall n. KnownNat n => ToJSON (Amount n) where
  toJSON = Number . fromRational . toRational . getAmount

instance forall n. KnownNat n => FromJSON (Amount n) where
  parseJSON (Number n) = pure $ Amount (fromRational (toRational n))
  parseJSON v = error $ "Expected Amount, saw: " ++ show v

-- KnownNat n => Prism' String (Amount n)
_Amount :: forall n p f. (KnownNat n, Choice p, Applicative f)
        => p (Amount n) (f (Amount n)) -> p String (f String)
_Amount = dimap read (fmap show)

-- (KnownNat m, KnownNat n) => Iso' (Amount n) (Amount m)
rounded :: forall m n p f. (Profunctor p, Functor f)
        => p (Amount m) (f (Amount m)) -> p (Amount n) (f (Amount n))
rounded = dimap coerce (fmap coerce)

amountToString :: forall n. KnownNat n => Amount n -> String
amountToString = cleanup 2 . showAmount mpfr_RNDNA
  where
    cleanup m t =
        let len = length (last (splitOn "." t)) in
        if len > m && last t == '0'
        then cleanup m (take (length t - 1) t)
        else t

thousands :: forall n. KnownNat n => Amount n -> String
thousands d = intercalate "." $ case splitOn "." str of
    x:xs -> (reverse . go . reverse) x :
        case xs of
            y:ys -> expand y : ys
            _ | isInt -> ["00"]
            _ -> []
    xs -> xs
  where
    isInt = case natVal (Proxy :: Proxy n) of
        0 -> True
        _ -> False

    str | isInt     = show (floor d :: Int)
        | otherwise = amountToString d

    go (x:y:z:[])    = x:y:z:[]
    go (x:y:z:['-']) = x:y:z:['-']
    go (x:y:z:xs)    = x:y:z:',':go xs
    go xs            = xs

    expand []     = "00"
    expand (x:[]) = x:"0"
    expand xs     = xs

renderAmount :: KnownNat n => Amount n -> String
renderAmount d
    | fromIntegral (floor d :: Int) == d
    = thousands @0 (coerce d)
renderAmount d = thousands d

normalizeAmount :: KnownNat n => CUInt -> Amount n -> Amount n
normalizeAmount = (read .) . showAmount

-- Given a way of project a "count" from an element, an amount, and a list of
-- elements, divide the given amount among the elements each according to its
-- count. Thus, if passed a two element list with counts 60 and 40, the amount
-- would be divided 60% to the first, and 40% to the second. The only wrinkle
-- is that any remaining cent from rounding is given to the first element.
spreadAmounts :: (KnownNat n, KnownNat m)
              => (a -> Amount m) -> Amount n -> [a] -> [(Amount n, a)]
spreadAmounts f n input = go True input
  where
    diff   = n - sum (map sumOfParts input)
    per    = coerce n / shares
    shares = sum (map f input)

    sumOfParts l = normalizeAmount mpfr_RNDZ (coerce (f l * per))

    go _ []     = []
    go b (x:xs) = (sum', x) : go False xs
      where
        sum' = normalizeAmount mpfr_RNDZ val
        val  = sumOfParts x + if b then diff else 0

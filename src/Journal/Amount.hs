{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Journal.Amount
  ( Amount (..),
    _Amount,
    rounded,
    thousands,
    renderAmount,
    normalizeAmount,
    spreadAmounts,
    showAmount,
    amountToString,
    mpfr_RNDN,
    mpfr_RNDZ,
    mpfr_RNDU,
    mpfr_RNDD,
    mpfr_RNDA,
    mpfr_RNDF,
    mpfr_RNDNA,
    sign,
  )
where

import Control.Monad
import Data.Aeson
import Data.Char (isDigit)
import Data.Coerce
import Data.Data
import Data.Default
import Data.Function (on)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.List.Split
import Data.Profunctor
import Data.Ratio
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import GHC.TypeLits
import Journal.Utils (Render (..))
import System.IO.Unsafe
import Text.PrettyPrint (text)
import Text.Show.Pretty as P
import Prelude hiding (Double, Float)

mpfr_RNDN, mpfr_RNDZ, mpfr_RNDU, mpfr_RNDD, mpfr_RNDA, mpfr_RNDF :: CUInt
mpfr_RNDNA :: CUInt
mpfr_RNDN = 0 -- round to nearest, with ties to even
mpfr_RNDZ = 1 -- round toward zero
mpfr_RNDU = 2 -- round toward +Inf
mpfr_RNDD = 3 -- round toward -Inf
mpfr_RNDA = 4 -- round away from zero
mpfr_RNDF = 5 -- faithful rounding

mpfr_RNDNA = 6 -- round to nearest, with ties away from zero (mpfr_round)

foreign import ccall unsafe "mpfr_free_str" c'mpfr_free_str :: CString -> IO ()

foreign import ccall unsafe "rational_to_str"
  c'rational_to_str ::
    CLong -> CULong -> CUInt -> CSize -> Ptr CString -> IO ()

newtype Amount (dec :: Nat) = Amount {getAmount :: Ratio Int64}
  deriving
    ( Generic,
      Data,
      Typeable,
      Ord,
      Num,
      Fractional,
      Real,
      RealFrac
    )

instance Default (Amount n) where
  def = 0

instance KnownNat n => PrettyVal (Amount n) where
  prettyVal = P.String . show

showAmount :: forall n. KnownNat n => CUInt -> Amount n -> String
showAmount rnd (Amount r) =
  unsafePerformIO $
    alloca $ \bufPtr -> do
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

instance KnownNat n => Eq (Amount n) where
  (==) = (==) `on` show

instance KnownNat n => Show (Amount n) where
  show = amountToString 2

instance KnownNat n => Read (Amount n) where
  readsPrec _d = \case
    '-' : xs -> map (\(x, y) -> (negate x, y)) (readNum xs)
    xs -> readNum xs
    where
      readNum r = case takeWhile isDigit r of
        [] -> error $ "Not an amount: " ++ r
        num -> case dropWhile isDigit r of
          ('.' : den) ->
            let den' = takeWhile isDigit den
                rem' = dropWhile isDigit den
             in [(Amount (read (num ++ den') % 10 ^ length den'), rem')]
          xs -> [(Amount (read num % 1), xs)]

instance KnownNat n => Render (Amount n) where
  rendered = text . show

instance KnownNat n => ToJSON (Amount n) where
  toJSON = Number . fromRational . toRational . getAmount

instance KnownNat n => FromJSON (Amount n) where
  parseJSON (Number n) = pure $ Amount (fromRational (toRational n))
  parseJSON v = error $ "Expected Amount, saw: " ++ show v

-- _Amount :: KnownNat n => Prism' String (Amount n)
_Amount ::
  (KnownNat n, Choice p, Applicative f) =>
  p (Amount n) (f (Amount n)) ->
  p String (f String)
_Amount = dimap read (fmap show)

-- rounded :: (KnownNat m, KnownNat n) => Iso' (Amount n) (Amount m)
rounded ::
  (Profunctor p, Functor f) =>
  p (Amount m) (f (Amount m)) ->
  p (Amount n) (f (Amount n))
rounded = dimap coerce (fmap coerce)

amountToString :: KnownNat n => Int -> Amount n -> String
amountToString n = touchup . cleanup n . showAmount mpfr_RNDNA
  where
    touchup s
      | last s == '.' = take (length s - 1) s
      | otherwise = s
    cleanup m t
      | len > m && last t == '0' && '.' `elem` t =
        cleanup m (take (length t - 1) t)
      | otherwise = t
      where
        len = length (last (splitOn "." t))

thousands :: forall n. KnownNat n => Amount n -> String
thousands d = intercalate "." $ case splitOn "." str of
  x : xs ->
    (reverse . go . reverse) x :
    case xs of
      y : ys -> expand y : ys
      _ | isInt -> ["00"]
      _ -> []
  xs -> xs
  where
    isInt = case natVal (Proxy :: Proxy n) of
      0 -> True
      _ -> False
    str
      | isInt = show (floor d :: Int)
      | otherwise = amountToString 2 d
    go (x : y : z : []) = x : y : z : []
    go (x : y : z : ['-']) = x : y : z : ['-']
    go (x : y : z : xs) = x : y : z : ',' : go xs
    go xs = xs
    expand [] = "00"
    expand (x : []) = x : "0"
    expand xs = xs

renderAmount :: KnownNat n => Amount n -> String
renderAmount d
  | fromIntegral (floor d :: Int) == d =
    thousands @0 (coerce d)
renderAmount d = thousands d

normalizeAmount :: KnownNat n => CUInt -> Amount n -> Amount n
normalizeAmount = (read .) . showAmount

-- Given a way of project a "count" from an element, an amount, and a list of
-- elements, divide the given amount among the elements each according to its
-- count. Thus, if passed a two element list with counts 60 and 40, the amount
-- would be divided 60% to the first, and 40% to the second.
spreadAmounts ::
  (KnownNat n, KnownNat m) =>
  (a -> Amount m) ->
  Amount n ->
  [a] ->
  [(Amount n, a)]
spreadAmounts f n input = go True input
  where
    diff = n - sum (map sump input)
    per = coerce n / shares
    shares = sum (map f input)
    sump l = coerce (f l * per)
    go _ [] = []
    go b (x : xs) = (sum', x) : go False xs
      where
        sum' = sump x + if b then diff else 0

sign :: (Num a, Ord a, Num b) => a -> b -> b
sign n = (if n < 0 then negate else id) . abs

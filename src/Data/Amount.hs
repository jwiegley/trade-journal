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
    ) where

import           Control.Lens
import           Data.Aeson
import           Data.Char (isDigit)
import           Data.Coerce
import           Data.Int (Int64)
import           Data.Proxy
import           Data.Ratio
import           Data.Text (Text)
import qualified Data.Text as T
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           GHC.TypeLits
import           Prelude hiding (Float, Double)
import           System.IO.Unsafe

foreign import ccall unsafe "rational_to_str" rational_to_str
    :: CLong -> CULong -> CSize -> CString -> IO ()

showAmount :: forall n. KnownNat n => Amount n -> String
showAmount (Amount r) =
    unsafePerformIO $ allocaBytes 256 $ \buf -> do
        rational_to_str
            (CLong (numerator r))
            (CULong (fromIntegral (denominator r)))
            (CSize (fromIntegral (natVal (Proxy :: Proxy n))))
            buf
        peekCString buf

newtype Amount (dec :: Nat) = Amount { getAmount :: Ratio Int64 }
    deriving (Ord, Num, Fractional, Real, RealFrac)

instance forall n. KnownNat n => Eq (Amount n) where
    x == y = show x == show y

instance forall n. KnownNat n => Show (Amount n) where
    show = T.unpack . amountToText

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
  toJSON = String . T.pack . show

instance forall n. KnownNat n => FromJSON (Amount n) where
  parseJSON (String s) = pure $ Amount (read (T.unpack s))
  parseJSON (Number n) = pure $ Amount (fromRational (toRational n))
  parseJSON v = error $ "Expected Amount, saw: " ++ show v

_Amount :: forall n. KnownNat n => Prism' Text (Amount n)
_Amount = prism' (T.pack . show) (read . T.unpack)

rounded :: forall n m. (KnownNat n, KnownNat m) => Iso' (Amount n) (Amount m)
rounded = iso coerce coerce

amountToText :: forall n. KnownNat n => Amount n -> Text
amountToText = cleanup 2 . T.pack . showAmount
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
        | otherwise = amountToText d

    go (x:y:z:[])    = x:y:z:[]
    go (x:y:z:['-']) = x:y:z:['-']
    go (x:y:z:xs)    = x:y:z:',':go xs
    go xs            = xs

    expand []     = "00"
    expand (x:[]) = x:"0"
    expand xs     = xs

renderAmount :: forall n. KnownNat n => Amount n -> Text
renderAmount d
    | fromIntegral (floor d :: Int) == d
    = thousands @0 (d^.from rounded)
renderAmount d = thousands d

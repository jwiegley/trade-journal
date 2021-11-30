{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Journal.Zippered where

import Control.Applicative
import Control.Arrow (first)
import Control.Lens
import Control.Monad
import GHC.Generics

-- A zippered list isn't exactly a Zipper, because it includes the "element of
-- focus" at the beginning of the suffix. It also cannot be a monoid, since
-- there can only be one point of focus.
data Zippered a = Zippered
  { _prefix :: [a],
    _suffix :: [a]
  }
  deriving (Show, Eq, Ord, Generic, Functor)

makeLenses ''Zippered

zippered :: (a -> Bool) -> [a] -> Zippered a
zippered f = uncurry Zippered . first reverse . break f

unzippered :: Zippered a -> [a]
unzippered Zippered {..} = reverse _prefix ++ _suffix

reverseZippered :: (a -> Bool) -> [a] -> Zippered a
reverseZippered f =
  uncurry Zippered
    . ( \(xs, ys) -> case ys of
          [] -> (xs, [])
          z : zs -> (zs, z : reverse xs)
      )
    . break f

reverseUnzippered :: Zippered a -> [a]
reverseUnzippered Zippered {..} = reverse _suffix ++ _prefix

items :: Traversal' (Zippered a) a
items f Zippered {..} =
  Zippered . reverse
    <$> traverse f (reverse _prefix)
    <*> traverse f _suffix

focus :: Traversal' (Zippered a) a
focus f z@Zippered {..} =
  case _suffix of
    [] -> pure z
    x : xs -> (\x' -> Zippered _prefix (x' : xs)) <$> f x

focii :: Lens' (Zippered a) [a]
focii f Zippered {..} =
  case _suffix of
    [] -> Zippered _prefix <$> f []
    x : xs -> (\x' -> Zippered _prefix (x' ++ xs)) <$> f [x]

-- | Given a zippered list, attempt to locate an element first in the prefix,
--   then in the suffix, and allow for a transformation of that sub-zippered
--   list within the parent list, plus the generation of some datum.
applyToPrefixOrSuffix ::
  (a -> Bool) ->
  (Bool -> Zippered a -> Maybe (Zippered a, b)) ->
  Zippered a ->
  Maybe (Zippered a, b)
applyToPrefixOrSuffix f g z =
  ( do
      guard (any f (z ^. prefix))
      (res, x) <- g True (reverseZippered f (z ^. prefix))
      pure (z & prefix .~ reverseUnzippered res, x)
  )
    <|> ( do
            guard (any f (z ^. suffix))
            (res, x) <- g False (zippered f (z ^. suffix))
            pure (z & suffix .~ unzippered res, x)
        )

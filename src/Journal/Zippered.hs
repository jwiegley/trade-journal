{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Journal.Zippered where

import Control.Lens
import GHC.Generics

-- A zippered list isn't exactly a Zipper, because it includes the "element of
-- focus" at the beginning of the suffix. It also cannot be a monoid, since
-- there can only be one point of focus.
data Zippered a = Zippered
  { _prefix :: [a],
    _suffix :: [a]
  }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

makeLenses ''Zippered

toList :: Zippered a -> [a]
toList Zippered {..} = reverse _prefix ++ _suffix

items :: Traversal' (Zippered a) a
items f Zippered {..} =
  Zippered . reverse
    <$> traverse f (reverse _prefix)
    <*> traverse f _suffix

zippered :: (a -> Bool) -> [a] -> Zippered a
zippered = (uncurry Zippered .) . break

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

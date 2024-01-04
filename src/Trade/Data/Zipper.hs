{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Trade.Data.Zipper where

import Control.Arrow (first)
import Control.Comonad
import Control.Lens hiding ((<.>))
import Control.Monad
import Data.Foldable
import Data.Functor.Apply
import Data.Functor.Extend
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup.Foldable
import GHC.Generics

data Zipper a = MkZipper {prefix :: [a], focus :: a, suffix :: [a]}

deriving instance Show a => Show (Zipper a)

deriving instance Eq a => Eq (Zipper a)

deriving instance Generic a => Generic (Zipper a)

deriving instance Functor Zipper

deriving instance Foldable Zipper

deriving instance Traversable Zipper

makeLenses ''Zipper

instance Applicative Zipper where
  pure a = MkZipper (repeat a) a (repeat a)
  MkZipper p1 f1 s1 <*> MkZipper p2 f2 s2 =
    MkZipper (zipWith id p1 p2) (f1 f2) (zipWith id s1 s2)

instance (Semigroup a) => Semigroup (Zipper a) where
  MkZipper p1 f1 s1 <> MkZipper p2 f2 s2 =
    MkZipper (zipWith (<>) p1 p2) (f1 <> f2) (zipWith (<>) s1 s2)

-- | Many of these instances are from Tony Morris's package, list-zipper
instance Apply Zipper where
  MkZipper l1 x1 r1 <.> MkZipper l2 x2 r2 =
    MkZipper (zipWith id l1 l2) (x1 x2) (zipWith id r1 r2)

instance Foldable1 Zipper where
  foldMap1 f (MkZipper [] x []) =
    f x
  foldMap1 f (MkZipper [] x (rh : rt)) =
    f x <> foldMap1 f (rh :| rt)
  foldMap1 f (MkZipper (lh : lt) x []) =
    foldMap1 f (lh :| lt) <> f x
  foldMap1 f (MkZipper (lh : lt) x (rh : rt)) =
    foldMap1 f (lh :| lt) <> f x <> foldMap1 f (rh :| rt)

instance Traversable1 Zipper where
  traverse1 f (MkZipper [] x []) =
    (\x' -> MkZipper [] x' []) <$> f x
  traverse1 f (MkZipper (lh : lt) x []) =
    (\l' x' -> MkZipper (toList l') x' []) <$> traverse1 f (lh :| lt) <.> f x
  traverse1 f (MkZipper [] x (rh : rt)) =
    (\x' r' -> MkZipper [] x' (toList r')) <$> f x <.> traverse1 f (rh :| rt)
  traverse1 f (MkZipper (lh : lt) x (rh : rt)) =
    (\l' x' r' -> MkZipper (toList l') x' (toList r'))
      <$> traverse1 f (lh :| lt)
      <.> f x
      <.> traverse1 f (rh :| rt)

instance Extend Zipper where
  duplicated z =
    let dup x = (x, x)
        unf f = unfoldr (fmap dup . f) z
     in MkZipper (unf left) z (unf right)

instance Comonad Zipper where
  duplicate = duplicated
  extract (MkZipper _ x _) = x

left :: Zipper a -> Maybe (Zipper a)
left (MkZipper [] _ _) = Nothing
left (MkZipper (x : p) f s) = Just (MkZipper p x (f : s))

right :: Zipper a -> Maybe (Zipper a)
right (MkZipper _ _ []) = Nothing
right (MkZipper p f (x : s)) = Just (MkZipper (f : p) x s)

fromList :: [a] -> Maybe (Zipper a)
fromList [] = Nothing
fromList (x : xs) = Just (MkZipper [] x xs)

unzipper :: Zipper a -> [a]
unzipper (MkZipper p f s) = reverse p ++ (f : s)

overlay :: Zipper a -> [a] -> Maybe (Zipper a)
overlay (MkZipper _ _ []) [] = Nothing
overlay (MkZipper xs _ (z : zs)) [] = Just (MkZipper xs z zs)
overlay (MkZipper xs _ zs) (w : ws) =
  Just (MkZipper xs w (ws ++ zs))

zipper :: MonadPlus f => (a -> Bool) -> [a] -> f (Zipper a)
zipper f xs =
  case break f xs of
    (ys, z : zs) -> return (MkZipper (reverse ys) z zs)
    _ -> mzero

spanM ::
  Monad m => MonadPlus f => (a -> m Bool) -> [a] -> m (f a, [a])
spanM _ [] = return (mzero, [])
spanM p (x : xs) =
  p x
    >>= \case
      True ->
        spanM p xs
          >>= \case
            (ys, zs) -> return (mplus (return x) ys, zs)
      False -> return (mzero, x : xs)

breakM ::
  Monad m => MonadPlus f => (a -> m Bool) -> [a] -> m (f a, [a])
breakM p = spanM $ return . not <=< p

zipperM ::
  Monad m => MonadPlus f => (a -> m Bool) -> [a] -> m (f (Zipper a))
zipperM k xs =
  breakM k xs
    <&> \case
      (ys, z : zs) -> return (MkZipper (reverse ys) z zs)
      _ -> mzero

items :: Traversal' (Zipper a) a
items k z =
  MkZipper
    <$> (reverse <$> traverse k (reverse (prefix z)))
    <*> k (focus z)
    <*> traverse k (suffix z)

scanPreState :: (a -> s -> (b, s)) -> s -> [a] -> [(b, s)]
scanPreState _ _ [] = []
scanPreState f s (x : xs) =
  case f x s of
    (b, s') -> (b, s) : scanPreState f s' xs

survey :: forall a. (Zipper a -> Zipper a) -> [a] -> [a]
survey f = maybe [] go . fromList
  where
    go :: Zipper a -> [a]
    go z = maybe (unzipper (f z)) go (right (f z))

surveyM :: forall m a. Monad m => (Zipper a -> m (Zipper a)) -> [a] -> m [a]
surveyM f = maybe (return []) go . fromList
  where
    go :: Zipper a -> m [a]
    go z = f z >>= \z' -> maybe (return (unzipper z')) go (right z')

mapUntils ::
  forall a b.
  ([a] -> [a]) ->
  (a -> Maybe ([a], b)) ->
  [a] ->
  Maybe ([a], b)
mapUntils _ _ [] = Nothing
mapUntils rev k (x : xs) =
  case k x of
    Just (xs', b) -> Just (rev xs' ++ xs, b)
    Nothing -> first (x :) <$> mapUntils rev k xs

mapLeftThenRightUntils ::
  Zipper a -> (Bool -> a -> Maybe ([a], b)) -> Maybe (Zipper a, b)
mapLeftThenRightUntils z f =
  case mapUntils reverse (f True) (prefix z) of
    Just (p', b) -> Just (MkZipper p' (focus z) (suffix z), b)
    Nothing -> case mapUntils id (f False) (suffix z) of
      Just (s', b) -> Just (MkZipper (prefix z) (focus z) s', b)
      Nothing -> Nothing

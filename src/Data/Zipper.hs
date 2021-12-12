{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Zipper where

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

data Zipper a = Zipper
  { _prefix :: [a],
    _focus :: a,
    _suffix :: [a]
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

makeLenses ''Zipper

-- | Many of these instances are from Tony Morris's package, list-zipper
instance Apply Zipper where
  Zipper l1 x1 r1 <.> Zipper l2 x2 r2 =
    Zipper (zipWith id l1 l2) (x1 x2) (zipWith id r1 r2)

instance Applicative Zipper where
  pure a = Zipper (repeat a) a (repeat a)
  (<*>) = (<.>)

instance Foldable1 Zipper where
  foldMap1 f (Zipper [] x []) =
    f x
  foldMap1 f (Zipper [] x (rh : rt)) =
    f x <> foldMap1 f (rh :| rt)
  foldMap1 f (Zipper (lh : lt) x []) =
    foldMap1 f (lh :| lt) <> f x
  foldMap1 f (Zipper (lh : lt) x (rh : rt)) =
    foldMap1 f (lh :| lt) <> f x <> foldMap1 f (rh :| rt)

instance Traversable1 Zipper where
  traverse1 f (Zipper [] x []) =
    (\x' -> Zipper [] x' []) <$> f x
  traverse1 f (Zipper (lh : lt) x []) =
    (\l' x' -> Zipper (toList l') x' []) <$> traverse1 f (lh :| lt) <.> f x
  traverse1 f (Zipper [] x (rh : rt)) =
    (\x' r' -> Zipper [] x' (toList r')) <$> f x <.> traverse1 f (rh :| rt)
  traverse1 f (Zipper (lh : lt) x (rh : rt)) =
    (\l' x' r' -> Zipper (toList l') x' (toList r'))
      <$> traverse1 f (lh :| lt) <.> f x <.> traverse1 f (rh :| rt)

instance Semigroup a => Semigroup (Zipper a) where
  Zipper l1 x1 r1 <> Zipper l2 x2 r2 =
    Zipper (zipWith (<>) l1 l2) (x1 <> x2) (zipWith (<>) r1 r2)

instance Extend Zipper where
  duplicated z =
    let dup x = (x, x)
        unf f = unfoldr (fmap dup . f) z
     in Zipper (unf left) z (unf right)

instance Comonad Zipper where
  duplicate = duplicated
  extract (Zipper _ x _) = x

left, right :: Zipper a -> Maybe (Zipper a)
left (Zipper (a : as) x bs) = Just (Zipper as a (x : bs))
left _ = Nothing
right (Zipper as x (b : bs)) = Just (Zipper (x : as) b bs)
right _ = Nothing

fromList :: [a] -> Maybe (Zipper a)
fromList [] = Nothing
fromList (x : xs) = Just (Zipper [] x xs)

unzipper :: Zipper a -> [a]
unzipper Zipper {..} = reverse _prefix ++ _focus : _suffix

overlay :: Zipper a -> [a] -> Zipper a
overlay (Zipper _ _ []) [] = error "Cannot overlay nothing into nothing"
overlay (Zipper xs _ (z : zs)) [] = Zipper xs z zs
overlay (Zipper xs _ zs) (w : ws) = Zipper xs w (ws ++ zs)

zipper :: MonadPlus f => (a -> Bool) -> [a] -> f (Zipper a)
zipper f xs = case break f xs of
  (ys, z : zs) -> pure (Zipper (reverse ys) z zs)
  _ -> mzero

spanM :: (Monad m, MonadPlus f) => (a -> m Bool) -> [a] -> m (f a, [a])
spanM _ [] = return (mzero, [])
spanM p (x : xs) = do
  bool <- p x
  if bool
    then do
      (ys, zs) <- spanM p xs
      return (pure x `mplus` ys, zs)
    else return (mzero, x : xs)

breakM :: (Monad m, MonadPlus f) => (a -> m Bool) -> [a] -> m (f a, [a])
breakM p = spanM $ return . not <=< p

zipperM ::
  (Monad m, MonadPlus f) =>
  (a -> m Bool) ->
  [a] ->
  m (f (Zipper a))
zipperM f xs =
  breakM f xs <&> \case
    (ys, z : zs) -> pure (Zipper (reverse ys) z zs)
    _ -> mzero

items :: Traversal' (Zipper a) a
items f Zipper {..} =
  Zipper . reverse
    <$> traverse f (reverse _prefix)
    <*> f _focus
    <*> traverse f _suffix

scanPreState :: (a -> s -> (b, s)) -> s -> [a] -> [(b, s)]
scanPreState f = go
  where
    go _ [] = []
    go s (x : xs) =
      let (b, s') = f x s
       in (b, s) : go s' xs

survey :: (Zipper a -> Zipper a) -> [a] -> [a]
survey f = maybe [] go . fromList
  where
    go z = let z' = f z in maybe (unzipper z') go (right z')

surveyM :: Monad m => (Zipper a -> m (Zipper a)) -> [a] -> m [a]
surveyM f = maybe (pure []) go . fromList
  where
    go z = do
      z' <- f z
      maybe (pure (unzipper z')) go (right z')

-- | Given a zipper list, attempt to locate an element first in the prefix,
--   then in the suffix, and allow for a transformation of that sub-zipper
--   list within the parent list, plus the generation of some datum.
mapLeftThenRightUntils ::
  Zipper a -> (Bool -> a -> Maybe ([a], b)) -> Maybe (Zipper a, b)
mapLeftThenRightUntils z f =
  case mapUntils reverse (f True) (z ^. prefix) of
    Just (p', b) -> Just (z & prefix .~ p', b)
    Nothing ->
      case mapUntils id (f False) (z ^. suffix) of
        Just (s', b) -> Just (z & suffix .~ s', b)
        Nothing -> Nothing
  where
    mapUntils rev k = go
      where
        go [] = Nothing
        go ((k -> Just (xs', b)) : xs) = Just (rev xs' ++ xs, b)
        go (x : xs) = first (x :) <$> go xs

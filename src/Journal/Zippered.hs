{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Journal.Zippered where

import Control.Applicative
import Control.Lens
import Control.Monad
import GHC.Generics

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

-- A zippered list isn't exactly a Zipper, because it includes the "element of
-- focus" at the beginning of the suffix. It also cannot be a monoid, since
-- there can only be one point of focus.
data Zippered a = Zippered
  { _prefix :: [a],
    _focus :: a,
    _suffix :: [a]
  }
  deriving (Show, Eq, Ord, Generic, Functor)

makeLenses ''Zippered

zippered :: MonadPlus f => (a -> Bool) -> [a] -> f (Zippered a)
zippered f xs = case break f xs of
  (ys, z : zs) -> pure (Zippered (reverse ys) z zs)
  _ -> mzero

zipperedM ::
  (Monad m, MonadPlus f) =>
  (a -> m Bool) ->
  [a] ->
  m (f (Zippered a))
zipperedM f xs =
  breakM f xs <&> \case
    (ys, z : zs) -> pure (Zippered (reverse ys) z zs)
    _ -> mzero

unzippered :: Zippered a -> [a]
unzippered Zippered {..} = reverse _prefix ++ _focus : _suffix

revSplit :: ([a], [a]) -> ([a], [a])
revSplit (xs, ys) = case ys of
  [] -> (xs, [])
  z : zs -> (zs, z : reverse xs)

reverseZippered :: MonadPlus f => (a -> Bool) -> [a] -> f (Zippered a)
reverseZippered f xs = case revSplit (break f xs) of
  (ys, z : zs) -> pure (Zippered ys z zs)
  _ -> mzero

reverseZipperedM ::
  (Monad m, MonadPlus f) =>
  (a -> m Bool) ->
  [a] ->
  m (f (Zippered a))
reverseZipperedM f xs =
  revSplit <$> breakM f xs <&> \case
    (ys, z : zs) -> pure (Zippered ys z zs)
    _ -> mzero

reverseUnzippered :: Zippered a -> [a]
reverseUnzippered Zippered {..} = reverse (_focus : _suffix) ++ _prefix

items :: Traversal' (Zippered a) a
items f Zippered {..} =
  Zippered . reverse
    <$> traverse f (reverse _prefix)
    <*> f _focus
    <*> traverse f _suffix

-- | Given a zippered list, attempt to locate an element first in the prefix,
--   then in the suffix, and allow for a transformation of that sub-zippered
--   list within the parent list, plus the generation of some datum.
applyToPrefixOrSuffix ::
  (Bool -> a -> Bool) ->
  (Bool -> Zippered a -> Maybe (Zippered a, b)) ->
  Zippered a ->
  Maybe (Zippered a, b)
applyToPrefixOrSuffix f g z =
  ( do
      r <- reverseZippered (f True) (z ^. prefix)
      (res, x) <- g True r
      pure (z & prefix .~ reverseUnzippered res, x)
  )
    <|> ( do
            r <- zippered (f False) (z ^. suffix)
            (res, x) <- g False r
            pure (z & suffix .~ unzippered res, x)
        )

applyToPrefixOrSuffixM ::
  Monad m =>
  (Bool -> a -> m Bool) ->
  (Bool -> Zippered a -> Maybe (Zippered a, b)) ->
  Zippered a ->
  m (Maybe (Zippered a, b))
applyToPrefixOrSuffixM f g z = do
  b <- do
    r' <- reverseZipperedM (f True) (z ^. prefix)
    pure $ do
      r <- r'
      (res, x) <- g True r
      pure (z & prefix .~ reverseUnzippered res, x)
  case b of
    Just _ -> pure b
    Nothing -> do
      r' <- zipperedM (f False) (z ^. suffix)
      pure $ do
        r <- r'
        (res, x) <- g False r
        pure (z & suffix .~ unzippered res, x)

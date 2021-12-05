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

paraBreak :: ([a] -> a -> [a] -> Bool) -> [a] -> ([a], [a])
paraBreak p = go []
  where
    go _ xs@[] = (xs, xs)
    go bs xs@(x : xs')
      | p bs x xs' = ([], xs)
      | otherwise = let (ys, zs) = go (bs ++ [x]) xs' in (x : ys, zs)

spanM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
spanM _ [] = return (mzero, [])
spanM p (x : xs) = do
  bool <- p x
  if bool
    then do
      (ys, zs) <- spanM p xs
      return (x : ys, zs)
    else return (mzero, x : xs)

breakM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
breakM p = spanM $ return . not <=< p

-- A zippered list isn't exactly a Zipper, because it includes the "element of
-- focus" at the beginning of the suffix. It also cannot be a monoid, since
-- there can only be one point of focus.
data Zippered a = Zippered
  { _prefix :: [a],
    _focii :: [a],
    _suffix :: [a]
  }
  deriving (Show, Eq, Ord, Generic, Functor)

makeLenses ''Zippered

zippered :: (a -> Bool) -> [a] -> Zippered a
zippered f = uncurry Zippered . first reverse . break f

zipperedM :: Monad m => (a -> m Bool) -> [a] -> m (Zippered a)
zipperedM f = fmap (uncurry Zippered . first reverse) . breakM f

paraZippered :: ([a] -> a -> [a] -> Bool) -> [a] -> Zippered a
paraZippered f = uncurry Zippered . first reverse . paraBreak f

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

reverseZipperedM :: Monad m => (a -> m Bool) -> [a] -> m (Zippered a)
reverseZipperedM f =
  fmap
    ( uncurry Zippered
        . ( \(xs, ys) -> case ys of
              [] -> (xs, [])
              z : zs -> (zs, z : reverse xs)
          )
    )
    . breakM f

reverseParaZippered :: ([a] -> a -> [a] -> Bool) -> [a] -> Zippered a
reverseParaZippered f =
  uncurry Zippered
    . ( \(xs, ys) -> case ys of
          [] -> (xs, [])
          z : zs -> (zs, z : reverse xs)
      )
    . paraBreak f

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
  (Bool -> a -> Bool) ->
  (Bool -> Zippered a -> Maybe (Zippered a, b)) ->
  Zippered a ->
  Maybe (Zippered a, b)
applyToPrefixOrSuffix f g z =
  ( do
      let r = reverseZippered (f True) (z ^. prefix)
      guard $ has focus r
      (res, x) <- g True r
      pure (z & prefix .~ reverseUnzippered res, x)
  )
    <|> ( do
            let r = zippered (f False) (z ^. suffix)
            guard $ has focus r
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
    r <- reverseZipperedM (f True) (z ^. prefix)
    pure $ do
      guard $ has focus r
      (res, x) <- g True r
      pure (z & prefix .~ reverseUnzippered res, x)
  case b of
    Just _ -> pure b
    Nothing -> do
      r <- zipperedM (f False) (z ^. suffix)
      pure $ do
        guard $ has focus r
        (res, x) <- g False r
        pure (z & suffix .~ unzippered res, x)

paraApplyToPrefixOrSuffix ::
  ([a] -> a -> [a] -> Bool) ->
  (Bool -> Zippered a -> Maybe (Zippered a, b)) ->
  Zippered a ->
  Maybe (Zippered a, b)
paraApplyToPrefixOrSuffix f g z =
  ( do
      guard $
        let (x, y) = paraBreak f (z ^. prefix)
         in not (null x || null y)
      (res, x) <- g True (reverseParaZippered f (z ^. prefix))
      pure (z & prefix .~ reverseUnzippered res, x)
  )
    <|> ( do
            guard $
              let (x, y) = paraBreak f (z ^. suffix)
               in not (null x || null y)
            (res, x) <- g False (paraZippered f (z ^. suffix))
            pure (z & suffix .~ unzippered res, x)
        )

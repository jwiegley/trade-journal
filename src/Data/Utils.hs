{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Data.Utils where

import           Control.Applicative
import           Control.Lens
import           Data.Foldable
import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude hiding (Float, Double, (<>))
import           Text.PrettyPrint as P

percent :: Num a => a -> Lens' a a
percent n f s = f part <&> \v -> v + (s - part)
  where part = s * n

zipped :: Traversal' s a -> Traversal' s b -> Traversal' s (a, b)
zipped f g k s = case liftA2 (,) (s^?f) (s^?g) of
    Nothing -> pure s
    Just p  -> k p <&> \(a, b) -> s & f .~ a & g .~ b

contractList :: (a -> a -> Maybe a) -> [a] -> [a]
contractList _ [] = []
contractList _ [x] = [x]
contractList f (x:y:xs) = case f x y of
    Nothing -> x : contractList f (y:xs)
    Just z  ->     contractList f (z:xs)

renderList :: (a -> Doc) -> [a] -> Doc
renderList _ [] = brackets P.empty
renderList f ts =
    fst (foldl' go (P.empty, True) ts) <> space <> rbrack
  where
    go (_, True) x    = (lbrack       <> space <> f x, False)
    go (acc, False) x = (acc $$ comma <> space <> f x, False)

-- A Fold over the individual components of a Text split on a separator.
--
-- splitOn :: Text -> Fold String String
-- splitOn :: Text -> Traversal' String String
--
-- splitOn :: Text -> IndexedFold Int String String
-- splitOn :: Text -> IndexedTraversal' Int String String
--
-- Note: This function type-checks as a Traversal but it doesn't satisfy the
-- laws. It's only valid to use it when you don't insert any separator strings
-- while traversing, and if your original Text contains only isolated split
-- strings.
splitOn :: Applicative f => Text -> IndexedLensLike' Int f Text Text
splitOn s f = fmap (T.intercalate s) . go . T.splitOn s
  where
    go = conjoined traverse (indexing traverse) f

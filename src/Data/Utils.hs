{-# LANGUAGE RankNTypes #-}

module Data.Utils where

import Control.Applicative
import Control.Lens
import Data.Foldable
import Prelude hiding (Float, Double, (<>))
import Text.PrettyPrint as P

zipped :: Traversal' s a -> Traversal' s b -> Traversal' s (a, b)
zipped f g k s = case liftA2 (,) (s^?f) (s^?g) of
    Nothing -> pure s
    Just p  -> k p <&> \(a, b) -> s & f .~ a & g .~ b

renderList :: (a -> Doc) -> [a] -> Doc
renderList _ [] = brackets P.empty
renderList f ts =
    fst (foldl' go (P.empty, True) ts) <> space <> rbrack
  where
    go (_, True) x    = (lbrack       <> space <> f x, False)
    go (acc, False) x = (acc $$ comma <> space <> f x, False)

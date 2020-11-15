{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Journal.Utils where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad (foldM)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Coerce
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601
import Debug.Trace (traceM)
import Text.PrettyPrint as P
import Prelude hiding (Double, Float, (<>))

readonly :: Monad m => ReaderT s m a -> StateT s m a
readonly f = lift . runReaderT f =<< get
{-# INLINE readonly #-}

justify :: [a] -> [Maybe a]
justify [] = [Nothing]
justify (x : xs) = Just x : justify xs
{-# INLINE justify #-}

unzipBoth :: [([a], [b])] -> ([a], [b])
unzipBoth = (concat *** concat) . unzip
{-# INLINE unzipBoth #-}

distance :: UTCTime -> UTCTime -> Integer
distance x y = abs (utctDay x `diffDays` utctDay y)
{-# INLINE distance #-}

renderM :: Applicative f => Doc -> f ()
renderM = traceM . render
{-# INLINE renderM #-}

percent :: (Num a, Num b, Coercible b a) => a -> Lens' b b
percent n f s = f part <&> \v -> v + (s - part)
  where
    part = s * coerce n
{-# INLINE percent #-}

zipped :: Traversal' s a -> Traversal' s b -> Traversal' s (a, b)
zipped f g k s = case liftA2 (,) (s ^? f) (s ^? g) of
  Nothing -> pure s
  Just p -> k p <&> \(a, b) -> s & f .~ a & g .~ b
{-# INLINE zipped #-}

zipped3 ::
  Traversal' s a ->
  Traversal' s b ->
  Traversal' s c ->
  Traversal' s (a, b, c)
zipped3 f g h k s = case liftA3 (,,) (s ^? f) (s ^? g) (s ^? h) of
  Nothing -> pure s
  Just p -> k p <&> \(a, b, c) -> s & f .~ a & g .~ b & h .~ c

contractList :: (a -> a -> Maybe a) -> [a] -> [a]
contractList _ [] = []
contractList _ [x] = [x]
contractList f (x : y : xs) = case f x y of
  Nothing -> x : contractList f (y : xs)
  Just z -> contractList f (z : xs)

-- | A specialized variant of 'foldM' with some arguments shifted around.
foldAM ::
  (Monad m, Applicative f) =>
  a ->
  [b] ->
  (b -> f a -> m (f a)) ->
  m (f a)
foldAM z xs f = foldM (flip f) (pure z) xs
{-# INLINE foldAM #-}

renderList :: (a -> Doc) -> [a] -> Doc
renderList _ [] = brackets P.empty
renderList f ts =
  fst (foldl' go (P.empty, True) ts) <> space <> rbrack
  where
    go (_, True) x = (lbrack <> space <> f x, False)
    go (acc, False) x = (acc $$ comma <> space <> f x, False)

class Render a where
  rendered :: a -> Doc

instance Render a => Render [a] where
  rendered = renderList rendered

instance Render a => Render (Maybe a) where
  rendered Nothing = text "Nothing"
  rendered (Just x) = text "Just" <> space <> rendered x

instance Render Text where
  rendered = text . T.unpack

instance Render Day where
  rendered = text . iso8601Show

instance Render UTCTime where
  rendered = text . iso8601Show . utctDay

tshow :: Show a => a -> Doc
tshow = text . show
{-# INLINE tshow #-}

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
splitOn s f =
  fmap (T.intercalate s)
    . conjoined traverse (indexing traverse) f
    . T.splitOn s
{-# INLINE splitOn #-}

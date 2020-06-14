{-# LANGUAGE BangPatterns #-}

module Data.Zipper where

-- A zipper represents the preceding and following elements of a list that is
-- missing an element. Note that both sub-lists begin with the element closest
-- to what is missing, so the first part is the reverse of the beginning of
-- the list.
data Zipper a
  = Zipper
      { _before :: [a],
        _after :: [a]
      }
  deriving (Eq, Show)

zipperCollapse :: Zipper a -> [a]
zipperCollapse (Zipper b a) = reverse b ++ a

zipperReconstitute :: a -> Zipper a -> [a]
zipperReconstitute x (Zipper b a) = reverse b ++ x : a

zipperInsert :: [a] -> Zipper a -> [a]
zipperInsert xs (Zipper b a) = reverse b ++ xs ++ a

zipperFoldl' :: (b -> a -> Zipper a -> b) -> b -> [a] -> b
zipperFoldl' f z = go [] z
  where
    go _ !r [] = r
    go p !r (x : rest) =
      go (x : p) (f r x (Zipper p rest)) rest

zipperFoldr :: (a -> Zipper a -> b -> b) -> b -> [a] -> b
zipperFoldr f z = go [] z
  where
    go _ r [] = r
    go p r (x : rest) =
      f x (Zipper p rest) (go (x : p) r rest)

zipperMapAccumLs' :: (b -> a -> Zipper a -> (b, [a])) -> b -> [a] -> (b, [a])
zipperMapAccumLs' f z = go [] z
  where
    go _ !r [] = (r, [])
    go p !r (x : rest) =
      (part ++) <$> go (x : p) r' rest
      where
        (r', part) = f r x (Zipper p rest)

mapAccumLs' :: (b -> a -> (b, [a])) -> b -> [a] -> (b, [a])
mapAccumLs' f z = go [] z
  where
    go _ !r [] = (r, [])
    go p !r (x : rest) =
      (part ++) <$> go (x : p) r' rest
      where
        (r', part) = f r x

focii :: [a] -> [(a, Zipper a)]
focii = zipperFoldr (\x z -> ((x, z) :)) []

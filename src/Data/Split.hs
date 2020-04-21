{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Split where

import Data.Default
import Data.List (foldl')
import Control.Lens

data Split a
    = Some
        { _used :: a
        , _kept :: a
        }
    | All a
    | None a
    deriving (Eq, Ord, Show)

makePrisms ''Split

instance Functor Split where
    fmap f (Some u k) = Some (f u) (f k)
    fmap f (All u)    = All (f u)
    fmap f (None k)   = None (f k)

_Splits :: Traversal (Split a) (Split b) a b
_Splits f (Some u k) = Some <$> f u <*> f k
_Splits f (All u)    = All <$> f u
_Splits f (None k)   = None <$> f k

_SplitUsed :: Traversal' (Split a) a
_SplitUsed f (Some u k) = Some <$> f u <*> pure k
_SplitUsed f (All u)    = All <$> f u
_SplitUsed _ (None k)   = pure $ None k

_SplitKept :: Traversal' (Split a) a
_SplitKept f (Some u k) = Some u <$> f k
_SplitKept _ (All u)    = pure $ All u
_SplitKept f (None k)   = None <$> f k

keepAll :: Split a -> [a]
keepAll (Some x y) = [x, y]
keepAll (All x)    = [x]
keepAll (None y)   = [y]

isFullTransfer :: (Maybe a, Split t) -> Bool
isFullTransfer (Nothing, All _) = True
isFullTransfer _ = False

data Applied v a b = Applied
    { _value :: v
    , _dest  :: Split a
    , _src   :: Split b
    }
    deriving (Eq, Ord, Show)

makeLenses ''Applied

nothingApplied :: Default v => a -> b -> Applied v a b
nothingApplied x y = Applied def (None x) (None y)

splits :: Default v => Split a -> Split b -> Applied v a b
splits = Applied def

data Considered a b c = Considered
    { _fromList    :: [a]
    , _newList     :: [b]
    , _fromElement :: [c]
    , _newElement  :: Maybe c
    }
    deriving (Eq, Show)

makeLenses ''Considered

newConsidered :: Considered a b c
newConsidered = Considered
    { _fromList    = []
    , _newList     = []
    , _fromElement = []
    , _newElement  = Nothing
    }

-- Given a list, and an element, determine the following three data:
--
-- - A revised version of the input list, based on that element
-- - Elements derived from the input list that become new outputs
-- - The fragments of the original element
consider :: (b -> c -> Applied v b c) -> (c -> v -> b -> a) -> [b] -> c
         -> Considered a b c
consider f mk lst el =
    result & fromList    %~ reverse
           & newList     %~ reverse
           & fromElement %~ reverse
           & newElement  .~ remaining
  where
    (remaining, result) = foldl' go (Just el, newConsidered) lst

    go (Nothing, c) x = (Nothing, c & newList %~ (x:))
    go (Just z,  c) x =
        ( _src^?_SplitKept
        , c & fromList    %~ maybe id ((:) . mk z _value) (_dest^?_SplitUsed)
            & newList     %~ maybe id (:) (_dest^?_SplitKept)
            & fromElement %~ maybe id (:) (_src^?_SplitUsed)
        )
      where
        Applied {..} = f x z

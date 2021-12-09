{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Journal.Types.Annotated where

import Control.Lens
import Data.Text (Text)
import Data.Time
import Data.Time.Format.ISO8601
import GHC.Generics hiding (to)
import Text.Show.Pretty
import Prelude hiding (Double, Float)

data Annotation
  = Note Text
  | Meta Text Text
  deriving (Show, PrettyVal, Eq, Ord, Generic)

makePrisms ''Annotation

instance PrettyVal UTCTime where
  prettyVal = String . iso8601Show

data Annotated a = Annotated
  { _item :: a,
    _time :: UTCTime,
    _account :: Text,
    -- | All annotations that relate to lot shares are expressed "per share".
    _details :: [Annotation]
  }
  deriving (Show, PrettyVal, Eq, Generic, Functor, Traversable, Foldable)

makeLenses ''Annotated

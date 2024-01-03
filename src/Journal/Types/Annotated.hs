{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Journal.Types.Annotated where

import Control.Lens hiding (Context)
import Data.Default
import Data.Text (Text)
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Time.Format.ISO8601
import GHC.Generics hiding (to)
import Text.Show.Pretty
import Prelude hiding (Double, Float)

data Annotation
  = Note !Text
  | Meta !Text !Text
  deriving (Show, PrettyVal, Eq, Ord, Generic)

makePrisms ''Annotation

instance PrettyVal UTCTime where
  prettyVal = String . iso8601Show

data Context = Context
  { _account :: !Text,
    _currency :: !Text
  }
  deriving (Show, PrettyVal, Eq, Generic)

makeLenses ''Context

instance Default Context where
  def =
    Context
      { _account = "",
        _currency = ""
      }

data Annotated a = Annotated
  { _item :: a,
    _time :: UTCTime,
    _context :: Context,
    -- | All annotations that relate to lot shares are expressed "per share".
    _details :: [Annotation]
  }
  deriving (Show, PrettyVal, Eq, Generic, Functor, Traversable, Foldable)

makeLenses ''Annotated

instance Default a => Default (Annotated a) where
  def =
    Annotated
      { _item = def,
        _time = UTCTime (fromOrdinalDate 2021 0) (secondsToDiffTime 0),
        _context = def,
        _details = []
      }

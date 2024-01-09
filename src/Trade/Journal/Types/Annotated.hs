{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Trade.Journal.Types.Annotated where

import Control.Lens hiding (Context)
import Data.Data
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
  deriving (Show, PrettyVal, Eq, Ord, Generic, Data)

makePrisms ''Annotation

instance PrettyVal UTCTime where
  prettyVal = String . iso8601Show

data Context = Context
  { _account :: !Text,
    _currency :: !Text
  }
  deriving (Show, PrettyVal, Eq, Generic, Data)

makeLenses ''Context

instance Default Context where
  def =
    Context
      { _account = "",
        _currency = ""
      }

data Annotated a = Annotated
  { _item :: !a,
    _time :: !UTCTime,
    _context :: !Context,
    _details :: ![Annotation]
  }
  deriving (Show, PrettyVal, Eq, Generic, Functor, Traversable, Foldable, Data)

makeLenses ''Annotated

instance Default a => Default (Annotated a) where
  def =
    Annotated
      { _item = def,
        _time = UTCTime (fromOrdinalDate 2021 0) (secondsToDiffTime 0),
        _context = def,
        _details = []
      }

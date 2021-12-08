{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Journal.Types.Annotated where

import Amount
import Control.Lens
import Data.Text (Text)
import Data.Time
import Data.Time.Format.ISO8601
import GHC.Generics hiding (to)
import Text.Show.Pretty
import Prelude hiding (Double, Float)

data Annotation
  = Fees (Amount 6) -- per share fee
  | Commission (Amount 6) -- per share commission
  | Ident Int
  | Order Text
  | Strategy Text
  | Account Text
  | Note Text
  | Meta Text Text
  deriving (Show, PrettyVal, Eq, Ord, Generic)

makePrisms ''Annotation

instance PrettyVal UTCTime where
  prettyVal = String . iso8601Show

data Annotated a = Annotated
  { _item :: a,
    _time :: UTCTime,
    -- | All annotations that relate to lot shares are expressed "per share",
    -- just like the price.
    _details :: [Annotation]
  }
  deriving (Show, PrettyVal, Eq, Generic, Functor, Traversable, Foldable)

makeLenses ''Annotated

fees :: Traversal' (Annotated a) (Amount 6)
fees = details . traverse . failing _Fees _Commission

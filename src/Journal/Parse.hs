module Journal.Parse where

import Text.Megaparsec
import Data.Text
import Data.Functor.Identity
import Data.Void

type Parser = ParsecT Void Text Identity

parse :: Parser ()
parse = pure ()

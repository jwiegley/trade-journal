{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Trade.Provider.Coinbase.Parser where

import Data.Csv qualified as Csv
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Vector (toList)
import Trade.Provider.Coinbase.Types

readCsv :: FilePath -> IO (Either Text [Transaction])
readCsv path = do
  text <- TL.readFile path
  let bs = TL.encodeUtf8 text
  case Csv.decodeByName bs of
    Left err -> fail $ "Error: " ++ err
    Right (_, res) -> pure $ Right $ toList res

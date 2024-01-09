{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Trade.Crypto.ICP where

import Amount
import Control.Applicative
import Control.Lens
import qualified Data.ByteString.Lazy as BL
import Data.Csv ((.:))
import qualified Data.Csv as Csv
import Data.Foldable
import Data.Int
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Time
import Data.Time.Format.ISO8601
import qualified Data.Vector as V
import GHC.Generics hiding (to)
import GHC.TypeLits (KnownNat)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read (readMaybe)
import Text.Show.Pretty
import Trade.Journal.Parse
import Trade.Journal.Print
import Trade.Journal.Types.Annotated
import Trade.Journal.Types.Entry
import Trade.Ledger
import Trade.Ledger.Entry ()
import Prelude hiding (Double, Float)

transactionFee :: Amount 6
transactionFee = 0.0001

type AccountId = Text

type NeuronId = Int64

data ManageNeuron
  = Stake {_stakeAmount :: !(Amount 8)}
  | Refresh {_refreshAmount :: !(Amount 8)}
  | AccrueMaturity {_accrueMaturityAmount :: !(Amount 8)}
  | MergeMaturity {_mergeMaturityAmount :: !(Amount 2)}
  | Spawn {_spawnedNeuron :: !NeuronId}
  | Disburse {_disburseAmount :: !(Amount 8)}
  | Split {_splitNeuron :: !NeuronId}
  | Merge {_mergeSource :: !NeuronId}
  deriving (Show, PrettyVal, Eq, Generic)

makePrisms ''ManageNeuron

data NeuronEntry = NeuronEntry
  { _neuron :: !NeuronId,
    _manage :: !ManageNeuron
  }
  deriving (Show, PrettyVal, Eq, Generic)

makeLenses ''NeuronEntry

-- | A fold over ICP transactions that yields the associated network fees.
--   These are constant for the Internet Computer and not dependent on the
--   transaction.
icpFees :: NeuronEntry -> Amount 6
icpFees icp = case icp ^. manage of
  Stake {} -> transactionFee
  Refresh {} -> transactionFee
  AccrueMaturity {} -> 0
  MergeMaturity {} -> 0
  Spawn {} -> transactionFee
  Disburse {} -> transactionFee
  Split {} -> transactionFee
  Merge {} -> transactionFee

icpNetAmount :: NeuronEntry -> Amount 2
icpNetAmount icp = case icp ^. manage of
  Stake amt -> -amt ^. coerced
  Refresh amt -> -amt ^. coerced
  AccrueMaturity {} -> 0
  MergeMaturity {} -> 0
  Spawn {} -> 0
  Disburse amt -> amt ^. coerced
  Split {} -> 0
  Merge {} -> 0

instance HasNetAmount NeuronEntry where
  _NetAmount f icp = error "never used" <$> f (icpNetAmount icp)

printNeuronEntry :: NeuronEntry -> TL.Text
printNeuronEntry (NeuronEntry n m) = case m of
  Stake amt ->
    "stake " <> tshow n <> " " <> printAmount 8 amt
  Refresh amt ->
    "refresh " <> tshow n <> " " <> printAmount 8 amt
  AccrueMaturity amt ->
    "accrue maturity " <> tshow n <> " " <> printAmount 8 amt
  MergeMaturity amt ->
    "merge maturity " <> tshow n <> " " <> printAmount 8 amt
  Spawn n2 ->
    "spawn " <> tshow n <> " -> " <> tshow n2
  Disburse amt ->
    "disburse " <> tshow n <> " " <> printAmount 8 amt
  Split n2 ->
    "split " <> tshow n <> " -> " <> tshow n2
  Merge n2 ->
    "merge " <> tshow n <> " <- " <> tshow n2
  where
    tshow = TL.pack . show

instance Printable NeuronEntry where
  printItem = printNeuronEntry

parseNeuronEntry :: Parser NeuronEntry
parseNeuronEntry = do
  keyword "stake"
    *> ( NeuronEntry
           <$> (L.decimal <* whiteSpace)
           <*> (Stake <$> parseAmount)
       )
    <|> keyword "refresh"
      *> ( NeuronEntry
             <$> (L.decimal <* whiteSpace)
             <*> (Refresh <$> parseAmount)
         )
    <|> keyword "accrue maturity"
      *> ( NeuronEntry
             <$> (L.decimal <* whiteSpace)
             <*> (AccrueMaturity <$> parseAmount)
         )
    <|> keyword "merge maturity"
      *> ( NeuronEntry
             <$> (L.decimal <* whiteSpace)
             <*> (MergeMaturity <$> parseAmount)
         )
    <|> keyword "spawn"
      *> ( NeuronEntry
             <$> (L.decimal <* whiteSpace)
             <*> (Spawn <$> L.decimal)
         )
    <|> keyword "disburse"
      *> ( NeuronEntry
             <$> (L.decimal <* whiteSpace)
             <*> (Disburse <$> parseAmount)
         )
    <|> keyword "split"
      *> ( NeuronEntry
             <$> (L.decimal <* whiteSpace)
             <*> (Split <$> L.decimal)
         )
    <|> keyword "merge"
      *> ( NeuronEntry
             <$> (L.decimal <* whiteSpace)
             <*> (Merge <$> L.decimal)
         )

_NeuronEntryLedgerRepr ::
  Fold (Annotated NeuronEntry) (Transaction (Annotated NeuronEntry) 8)
_NeuronEntryLedgerRepr f ann =
  fmap (error "Never reached") . f $ case ann ^. item . manage of
    Stake _amt -> undefined
    Refresh _amt -> undefined
    AccrueMaturity _amt -> undefined
    MergeMaturity _amt -> undefined
    Spawn _n2 -> undefined
    Disburse _amt -> undefined
    Split _n2 -> undefined
    Merge _n2 -> undefined

data ICPPrice = ICPPrice
  { icpDate :: !Day,
    icpPrice :: !(Amount 6)
  }
  deriving (Generic, Eq, Show)

readAmount :: KnownNat n => String -> Amount n
readAmount "" = 0
readAmount ('(' : xs) = -(readAmount xs)
readAmount s = case readMaybe (filter (`notElem` [',', '$', ')']) s) of
  Nothing -> error $ "Failed to read amount: " ++ s
  Just x -> x

instance Csv.FromNamedRecord ICPPrice where
  parseNamedRecord m =
    ICPPrice
      <$> (iso8601ParseM =<< m .: "Date")
      <*> (readAmount <$> m .: "Price")

readICPPrices :: FilePath -> IO ()
readICPPrices path = do
  putStrLn $ "Reading ICP prices from " ++ path
  eres <- Csv.decodeByName <$> BL.readFile path
  case eres of
    Left err -> error $ "Error " ++ show err
    Right (_header, prices :: V.Vector ICPPrice) ->
      forM_ (V.toList prices) print

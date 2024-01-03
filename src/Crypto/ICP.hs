{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Crypto.ICP where

import Amount
import Control.Applicative
import Control.Lens
import Data.Int
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import GHC.Generics hiding (to)
import Journal.Parse
import Journal.Print
import Journal.Types.Annotated
import Journal.Types.Entry
import Ledger
import Ledger.Entry ()
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Show.Pretty
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

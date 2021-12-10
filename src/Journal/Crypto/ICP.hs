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

module Journal.Crypto.ICP where

import Amount
import Control.Applicative
import Control.Lens
import Data.Int
import Data.Sum.Lens
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

data ICP
  = Stake
      { _stakeNeuron :: NeuronId,
        _stakeAmount :: Amount 8
      }
  | Refresh
      { _refreshNeuron :: NeuronId,
        _refreshAmount :: Amount 8
      }
  | AccrueMaturity
      { -- usually inferred
        _accrueMaturityNeuron :: NeuronId,
        _accrueMaturityAmount :: Amount 8
      }
  | MergeMaturity
      { _mergeMaturityNeuron :: NeuronId,
        _mergeMaturityAmount :: Amount 2
      }
  | Spawn
      { _spawnNeuron :: NeuronId,
        _spawnCreated :: NeuronId
      }
  | Disburse
      { _disburseNeuron :: NeuronId,
        _disburseAmount :: Amount 8
      }
  | Split
      { _splitNeuron :: NeuronId,
        _splitCreated :: NeuronId
      }
  | Merge
      { _mergeTarget :: NeuronId,
        _mergeSource :: NeuronId
      }
  deriving (Show, PrettyVal, Eq, Generic)

makePrisms ''ICP

-- | A fold over ICP transactions that yields the associated network fees.
--   These are constant for the Internet Computer and not dependent on the
--   transaction.
icpFees :: Fold ICP (Amount 6)
icpFees f =
  fmap (error "Never reached") . f . \case
    Stake {} -> transactionFee
    Refresh {} -> transactionFee
    AccrueMaturity {} -> 0
    MergeMaturity {} -> 0
    Spawn {} -> transactionFee
    Disburse {} -> transactionFee
    Split {} -> transactionFee
    Merge {} -> transactionFee

_ICPNetAmount :: Fold ICP (Amount 2)
_ICPNetAmount f =
  fmap (error "Never reached") . f . view coerced . \case
    Stake _ amt -> - amt
    Refresh _ amt -> - amt
    AccrueMaturity {} -> 0
    MergeMaturity {} -> 0
    Spawn {} -> 0
    Disburse _ amt -> amt
    Split {} -> 0
    Merge {} -> 0

instance HasNetAmount (Const ICP) where
  _NetAmount f (Const s) = fmap Const $ s & _ICPNetAmount %%~ f

printICP :: ICP -> TL.Text
printICP = \case
  Stake n amt ->
    "stake " <> tshow n <> " " <> printAmount 8 amt
  Refresh n amt ->
    "refresh " <> tshow n <> " " <> printAmount 8 amt
  AccrueMaturity n amt ->
    "accrue maturity " <> tshow n <> " " <> printAmount 8 amt
  MergeMaturity n amt ->
    "merge maturity " <> tshow n <> " " <> printAmount 8 amt
  Spawn n1 n2 ->
    "spawn " <> tshow n1 <> " -> " <> tshow n2
  Disburse n amt ->
    "disburse " <> tshow n <> " " <> printAmount 8 amt
  Split n1 n2 ->
    "split " <> tshow n1 <> " -> " <> tshow n2
  Merge n1 n2 ->
    "merge " <> tshow n1 <> " <- " <> tshow n2
  where
    tshow = TL.pack . show

instance Printable (Const ICP) where
  printItem = printICP . getConst

parseICP :: Parser ICP
parseICP = do
  keyword "stake"
    *> (Stake <$> (L.decimal <* whiteSpace) <*> parseAmount)
    <|> keyword "refresh"
      *> (Refresh <$> (L.decimal <* whiteSpace) <*> parseAmount)
    <|> keyword "accrue maturity"
      *> (AccrueMaturity <$> (L.decimal <* whiteSpace) <*> parseAmount)
    <|> keyword "merge maturity"
      *> (MergeMaturity <$> (L.decimal <* whiteSpace) <*> parseAmount)
    <|> keyword "spawn"
      *> (Spawn <$> (L.decimal <* whiteSpace) <*> L.decimal)
    <|> keyword "disburse"
      *> (Disburse <$> (L.decimal <* whiteSpace) <*> parseAmount)
    <|> keyword "split"
      *> (Split <$> (L.decimal <* whiteSpace) <*> L.decimal)
    <|> keyword "merge"
      *> (Merge <$> (L.decimal <* whiteSpace) <*> L.decimal)

instance Producible Parser (Const ICP) where
  produce = fmap Const parseICP

_ICPLedgerRepr :: Fold (Annotated ICP) (Transaction (Annotated ICP) 8)
_ICPLedgerRepr f ann =
  fmap (error "Never reached") . f $ case ann ^. item of
    Stake _n _amt -> undefined
    Refresh _n _amt -> undefined
    AccrueMaturity _n _amt -> undefined
    MergeMaturity _n _amt -> undefined
    Spawn _n1 _n2 -> undefined
    Disburse _n _amt -> undefined
    Split _n1 _n2 -> undefined
    Merge _n1 _n2 -> undefined

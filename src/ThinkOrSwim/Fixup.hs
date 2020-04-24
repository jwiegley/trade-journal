{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module ThinkOrSwim.Fixup (fixupTransaction) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Data.Amount
import           Data.Coerce
import           Data.Ledger (account, postings, _CommodityAmount, kind,
                              Account(..))
import qualified Data.Ledger as L
import           Prelude hiding (Float, Double, (<>))
import qualified ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import           ThinkOrSwim.Transaction
import           ThinkOrSwim.Transaction.Instances ()
import           ThinkOrSwim.Types

fixupTransaction
    :: L.Transaction API.TransactionSubType API.Order API.Transaction
    -> State (GainsKeeperState API.TransactionSubType API.Transaction)
            (L.Transaction API.TransactionSubType API.Order API.Transaction)
fixupTransaction t | not xactOA = pure t
  where
    xactOA = Prelude.any optionA (t^.postings)
           && Prelude.any equityA (t^.postings)

    hasOA = has (L.amount._CommodityAmount.kind.API._OptionAssignment)

    optionA p = hasOA p &&
        has (L.amount._CommodityAmount.L.instrument.L._Option) p
    equityA p = hasOA p &&
        has (L.amount._CommodityAmount.L.instrument.L._Equity) p

fixupTransaction t = t & postings %%~ fmap fst . foldM go ([], 0) . reverse
  where
    go (ps, cst) p
        | p^.account `elem` [ CapitalGainShort, CapitalGainLong
                       , CapitalLossShort, CapitalLossLong
                       , CapitalWashLoss ] =
          pure (ps, p^?!L.amount.L._DollarAmount)

        | L.Equities _ <- p^.account = do
          let l = p^?!L.amount._CommodityAmount
          if l^.quantity > 0
              then do
                  let l' = l & L.cost.mapped +~ coerce cst
                      p' = p & L.amount._CommodityAmount .~ l'
                  zoom (openTransactions.at (l^.L.symbol)) $
                      _Just.traverse %= \x -> if x == l then l' else x
                  pure (p':ps, 0)
              else do
                  -- jww (2020-04-24): Might be long-term gains.
                  let p' = L.newPosting
                          (if cst > 0
                           then CapitalLossShort
                           else CapitalGainShort)
                          False (L.DollarAmount (coerce (calcGain l) + cst + fees))
                  pure (p':p:ps, 0)

        | otherwise = pure (p:ps, cst)

    calcGain l = l^.cost + l^.quantity * l^.price

    fees = sum $
        t ^.. postings.traverse
            . filtered ((`elem` [ Fees, Charges, Commissions ]) . view account)
            . L.amount.L._DollarAmount

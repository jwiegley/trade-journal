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

module Journal.Entry where

import Amount
import Control.Applicative
import Control.Lens
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import GHC.Generics hiding (to)
import Journal.Parse
import Journal.Print
import Data.Sum.Lens
import Journal.Types.Entry
import Journal.Types.Lot
import Text.Show.Pretty
import Prelude hiding (Double, Float)

-- | An Event represents "internal events" that occur within an account,
--   either directly due to the actions above, or indirectly because of other
--   factors.
data Entry
  = Deposit (Amount 2) -- deposit money into the account
  | Withdraw (Amount 2) -- withdraw money from the account
  --
  | TransferIn Lot -- buy securities using money in the account
  | TransferOut Lot -- sell securities for a loss or gain
  --
  | Exercise Lot -- exercise a long options position
  | Assign Lot -- assignment of a short options position
  | Expire Lot -- expiration of a short options position
  --
  | Dividend (Amount 2) Lot -- dividend paid on a long position
  | Interest (Amount 2) (Maybe Text) -- interest earned
  --
  | Income (Amount 2) -- taxable income earned
  | Credit (Amount 2) -- account credit received
  deriving
    ( Show,
      PrettyVal,
      Eq,
      Generic
    )

makePrisms ''Entry

_EntryLot :: Traversal' Entry Lot
_EntryLot f = \case
  Deposit amt -> pure $ Deposit amt
  Withdraw amt -> pure $ Withdraw amt
  TransferIn lot -> TransferIn <$> f lot
  TransferOut lot -> TransferOut <$> f lot
  Exercise lot -> Exercise <$> f lot
  Assign lot -> Assign <$> f lot
  Expire lot -> Expire <$> f lot
  Dividend amt lot -> Dividend amt <$> f lot
  Interest amt sym -> pure $ Interest amt sym
  Income amt -> pure $ Income amt
  Credit amt -> pure $ Credit amt

instance HasLot (Const Entry) where
  _Lot f (Const s) = fmap Const $ s & _EntryLot %%~ f

_EntryNetAmount :: Fold Entry (Amount 2)
_EntryNetAmount f = \case
  Deposit amt -> Deposit <$> f amt
  Withdraw amt -> Deposit <$> f (- amt)
  TransferIn _lot -> Deposit <$> f 0
  TransferOut _lot -> Deposit <$> f 0
  Exercise _lot -> Deposit <$> f 0 -- jww (2021-06-12): NYI
  Assign _lot -> Deposit <$> f 0 -- jww (2021-06-12): NYI
  Expire _lot -> Deposit <$> f 0
  Dividend amt _lot -> Deposit <$> f amt
  Interest amt _sym -> Deposit <$> f amt
  Income amt -> Deposit <$> f amt
  Credit amt -> Deposit <$> f amt

instance HasNetAmount (Const Entry) where
  _NetAmount f (Const s) = fmap Const $ s & _EntryNetAmount %%~ f

printEntry :: Entry -> TL.Text
printEntry = \case
  Deposit amt -> "deposit " <> printAmount 2 amt
  Withdraw amt -> "withdraw " <> printAmount 2 amt
  TransferIn lot -> "xferin " <> printLot lot
  TransferOut lot -> "xferout " <> printLot lot
  Exercise lot -> "exercise " <> printLot lot
  -- Open pos -> "open " <> printPosition pos
  -- Close cl -> "close " <> printClosing cl
  Assign lot -> "assign " <> printLot lot
  Expire lot -> "expire " <> printLot lot
  Dividend amt lot -> "dividend " <> printAmount 2 amt <> " " <> printLot lot
  Interest amt Nothing -> "interest " <> printAmount 2 amt
  Interest amt (Just sym) ->
    "interest " <> printAmount 2 amt <> " from " <> printString sym
  Income amt -> "income " <> printAmount 2 amt
  Credit amt -> "credit " <> printAmount 2 amt

instance Printable (Const Entry) where
  printItem = printEntry . getConst

parseEntry :: Parser Entry
parseEntry =
  keyword "deposit" *> (Deposit <$> parseAmount)
    <|> keyword "withdraw" *> (Withdraw <$> parseAmount)
    -- <|> keyword "buy" *> (Buy <$> parseLot)
    -- <|> keyword "sell" *> (Sell <$> parseLot)
    <|> keyword "xferin" *> (TransferIn <$> parseLot)
    <|> keyword "xferout" *> (TransferOut <$> parseLot)
    <|> keyword "exercise" *> (Exercise <$> parseLot)
    -- <|> keyword "open" *> (Open <$> parsePosition parseData)
    -- <|> keyword "close" *> (Close <$> parseClosing parseData)
    <|> keyword "assign" *> (Assign <$> parseLot)
    <|> keyword "expire" *> (Expire <$> parseLot)
    <|> keyword "dividend" *> (Dividend <$> parseAmount <*> parseLot)
    <|> keyword "interest"
      *> ( Interest <$> parseAmount
             <*> optional (keyword "from" *> (TL.toStrict <$> parseSymbol))
         )
    <|> keyword "income" *> (Income <$> parseAmount)
    <|> keyword "credit" *> (Credit <$> parseAmount)

instance Producible Parser (Const Entry) where
  produce = fmap Const parseEntry

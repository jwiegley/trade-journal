{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Taxes.USA.WashSaleRule where

import Amount
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Functor.Classes
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Maybe (isJust, isNothing)
import Data.Sum
import Data.Sum.Lens
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Time
import Data.Zipper
import GHC.Generics
import Journal.Closings
import Journal.Parse
import Journal.Print
import Journal.Types
import Journal.Utils (distance, sideline)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Show.Pretty hiding (Time)

data Period = Past | Future
  deriving (Show, PrettyVal, Eq, Ord, Enum, Bounded, Generic)

makePrisms ''Period

data Washing
  = Exempt
  | WashTo Text (Maybe (Amount 6, Amount 6))
  | WashApply Text (Amount 6) -- per share wash applied
  | Wash
      { _washPeriod :: Period,
        _washPositionIdent :: Int,
        _washCostBasis :: Amount 6,
        _washClosing :: Closing
      }
  | WashedFrom
      { _washedFromPeriod :: Period,
        _washedFromClosingLot :: Lot,
        _washedFromCostBasis :: Amount 6,
        _washedPosition :: Position
      }
  deriving (Show, PrettyVal, Eq, Ord, Generic)

makePrisms ''Washing

instance HasPositionEvent (Const Washing) where
  _Event f (Const s) =
    Const <$> case s of
      Exempt -> pure Exempt
      WashTo t m -> pure $ WashTo t m
      WashApply t amt -> pure $ WashApply t amt
      Wash p i a c -> Wash p i a <$> (f (Close c) <&> (^?! _Close))
      WashedFrom p l a p' ->
        WashedFrom p l a <$> (f (Open p') <&> (^?! _Open))

_WashingLot :: Traversal' Washing Lot
_WashingLot f = \case
  Exempt -> pure Exempt
  WashTo t m -> pure $ WashTo t m
  WashApply t amt -> pure $ WashApply t amt
  Wash p i a c -> Wash p i a <$> (c & closingLot %%~ f)
  WashedFrom p l a p' -> WashedFrom p l a <$> (p' & posLot %%~ f)

instance HasLot (Const Washing) where
  _Lot f (Const s) = fmap Const $ s & _WashingLot %%~ f

type Washable r v =
  ( HasTraversal' HasPositionEvent r,
    HasTraversal' HasPositionEvent (Const Washing ': r),
    Const PositionEvent :< r,
    Apply Eq1 (Const Washing ': r),
    Apply Eq1 r,
    Eq v,
    Apply Show1 (Const Washing ': r),
    Apply Show1 r,
    Show v
  )

-- This implementation of the wash sale rule requires that we know the total
-- set of broker events in advance. Basically, each time we encounter a
-- non-exempt position open, we check 30 days back and then 30 days forward
-- for an eligible losing close; if it exists, the loss is moved into the cost
-- basis of the open.
washSaleRule ::
  Washable r v =>
  [Annotated (Sum r v)] ->
  [Annotated (Sum (Const Washing ': r) v)]
washSaleRule =
  sideline (isNothing . (^? item . projectedC . _Exempt)) go
    . map (fmap weaken)
  where
    go ::
      Washable r v =>
      [(Bool, Annotated (Sum (Const Washing ': r) v))] ->
      [(Bool, Annotated (Sum (Const Washing ': r) v))]
    go xs = maybe xs go do
      -- The wash sale rule looks for losing sales that haven't yet been
      -- washed. If there are none, we are done.
      let (z0, poss) = eligibleClose xs
      z1 <- z0
      x <- z1 ^? focus
      c <- x ^? _2 . item . _Event . _Close

      -- Next we look for an eligible opening 30 days before or after that
      -- sale. If there isn't one, we are done.
      (z2, x') <-
        flip evalState poss $
          applyToPrefixOrSuffixM
            ( eligibleOpen
                (c ^. closingLot . symbol)
                (c ^. closingIdent)
                (x ^. _2 . time)
            )
            (handleOpen x)
            z1

      let z3 = z2 & focus .~ x'
      guard $ z1 /= z3
      pure $ unzipper z3

    losses :: Position -> Closing -> Amount 6
    losses pos c =
      let p = c ^. closingLot . price
          b = pos ^. posLot . price
       in case pos ^. posDisp of
            Long -> p - b
            Short -> b - p

    eligibleClose ::
      Washable r v =>
      [(Bool, Annotated (Sum (Const Washing ': r) v))] ->
      ( Maybe
          ( Zipper
              ( Bool,
                Annotated (Sum (Const Washing ': r) v)
              )
          ),
        Map Text (IntMap (Annotated (Sum r v)))
      )
    eligibleClose xs = flip runState mempty $
      flip zipperM xs $ \(w, x) -> do
        poss <- get
        forM_ (x ^? item . _Event) $ \e ->
          put $ positionsFromEntry poss (inject (Const e) <$ x)
        pure $
          w && Just True == do
            c <- x ^? item . _Event . _Close
            pos <-
              poss
                ^? ix (c ^. closingLot . symbol)
                  . ix (c ^. closingIdent)
                  . item
                  . _Event
                  . _Open
            guard $ losses pos c < 0
            pure True

    eligibleOpen ::
      Washable r v =>
      Text ->
      Int ->
      UTCTime ->
      Bool ->
      (Bool, Annotated (Sum (Const Washing ': r) v)) ->
      State (Map Text (IntMap (Annotated (Sum r v)))) Bool
    eligibleOpen sym ident anchor inPast (w, y) = do
      poss <- get
      pure $
        w && Just True == do
          o <- y ^? item . _Event . _Open
          guard $ o ^. posLot . symbol == sym
          guard $ o ^. posIdent /= ident
          guard $ not inPast || isJust (poss ^? ix sym . ix (o ^. posIdent))
          guard $ abs (anchor `distance` (y ^. time)) <= 30
          pure True

    handleOpen ::
      Washable r v =>
      (Bool, Annotated (Sum (Const Washing ': r) v)) ->
      Bool ->
      Zipper (Bool, Annotated (Sum (Const Washing ': r) v)) ->
      Maybe
        ( Zipper (Bool, Annotated (Sum (Const Washing ': r) v)),
          (Bool, Annotated (Sum (Const Washing ': r) v))
        )
    handleOpen x inPast part = do
      c <- x ^? _2 . item . _Event . _Close
      y <- part ^? focus
      o <- y ^? _2 . item . _Event . _Open

      let loss =
            (losses o c * c ^. closingLot . amount) -- the total loss
              / (o ^. posLot . amount)
          o' =
            WashedFrom
              { _washedFromPeriod =
                  if inPast
                    then Future
                    else Past,
                _washedFromClosingLot = c ^. closingLot,
                _washedFromCostBasis = o ^. posLot . price,
                _washedPosition =
                  -- losses increase cost basis
                  o & posLot . price -~ loss
              }
          c' =
            Wash
              { _washPeriod =
                  if inPast
                    then Past
                    else Future,
                _washPositionIdent = o ^. posIdent,
                _washCostBasis = o ^. posLot . price,
                _washClosing = c
              }
      pure
        ( part & focus
            .~ ( y & _1 .~ False
                   & _2 . item .~ (projectedC # o')
               ),
          x & _1 .~ False
            & _2 . item .~ (projectedC # c')
        )

instance Printable (Const Washing) where
  printItem = either id id . printWashing . getConst

printWashing :: Washing -> Either TL.Text TL.Text
printWashing = \case
  -- WashedFrom Past _ _ x -> Right $ "washed from past " <> printAmount 2 x
  -- WashedFrom Future _ _ x -> Right $ "washed from future " <> printAmount 2 x
  WashTo x (Just (q, p)) ->
    Left $
      "wash "
        <> printAmount 0 q
        <> " @ "
        <> printAmount 4 p
        <> " to "
        <> TL.fromStrict x
  WashTo x Nothing -> Left $ "wash to " <> TL.fromStrict x
  WashApply x amt ->
    Left $ "apply " <> TL.fromStrict x <> " " <> printAmount 0 amt
  Exempt -> Left "exempt"

parseWashing :: Parser Washing
parseWashing =
  -- keyword "wash"
  --   *> keyword "from"
  --   *> keyword "past"
  --   *> (WashedFromPast <$> parseTime <*> parseLot)
  --   <|> keyword "wash"
  --     *> keyword "from"
  --     *> keyword "future"
  --     *> (WashedFromFuture <$> parseTime <*> parseLot)
  -- <|> keyword "washed" *> (Washed <$> parseAmount)
  -- <|> keyword "wash" *> parseWash
  keyword "apply"
    *> (WashApply . TL.toStrict <$> parseSymbol <*> parseAmount)
    <|> (Exempt <$ keyword "exempt")
  where
    parseWash = do
      mres <- optional do
        q <- parseAmount
        _ <- char '@' <* whiteSpace
        p <- parseAmount
        pure (q, p)
      _ <- keyword "to"
      sym <- parseSymbol
      pure $ WashTo (TL.toStrict sym) mres

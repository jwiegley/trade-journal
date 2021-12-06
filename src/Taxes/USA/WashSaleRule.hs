{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Taxes.USA.WashSaleRule where

import Amount
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Functor
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Time
-- import Debug.Trace
import GHC.Generics
import Journal.Closings (positionsFromEntry)
import Journal.Parse
import Journal.Print
import Journal.Types
import Journal.Utils (distance, sideline)
import Journal.Zippered
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
        _washCostBasis :: Amount 6
      }
  | WashedFrom
      { _washedFromPeriod :: Period,
        _washedFromClosingLot :: Lot,
        _washedFromCostBasis :: Amount 6
      }
  deriving (Show, PrettyVal, Eq, Ord, Generic)

makePrisms ''Washing

-- This implementation of the wash sale rule requires that we know the total
-- set of broker events in advance. Basically, each time we encounter a
-- non-exempt position open, we check 30 days back and then 30 days forward
-- for an eligible losing close; if it exists, the loss is moved into the cost
-- basis of the open.
washSaleRule :: [Annotated (Entry [Washing])] -> [Annotated (Entry [Washing])]
washSaleRule =
  sideline
    ( null
        . ( ^..
              failing
                (opening . posData)
                (closing . closingData)
                . traverse
                . _Exempt
          )
    )
    go
  where
    go ::
      [(Bool, Annotated (Entry [Washing]))] ->
      [(Bool, Annotated (Entry [Washing]))]
    go xs = maybe xs go do
      -- The wash sale rule proceeds by looking for losing sales that haven't
      -- been washed. If there are none, we are done.
      let (z0, poss) = eligibleClose xs
      z1 <- z0
      x <- z1 ^? focus
      c <- x ^? _2 . closing
      -- traceM $ "::: go.x =\n" ++ ppShow x
      -- traceM $ "::: go.poss =\n" ++ ppShow poss
      -- traceM $ "::: go.z1 prefix =\n" ++ ppShow (z1 ^. prefix)
      -- traceM $ "::: go.z1 focus =\n" ++ ppShow (z1 ^? focus)
      -- traceM $ "::: go.z1 suffix =\n" ++ ppShow (z1 ^. suffix)

      -- Once an eligible losing sale is found, we look for an eligible
      -- opening within 30 days before or after that sale. If there isn't one,
      -- or if the following action results in no change to the set of washed
      -- openings, we are done.
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
      -- traceM $ "::: go.x' =\n" ++ ppShow x'

      let z3 = z2 & focus .~ x'
      guard $ z1 /= z3
      pure $ unzippered z3

    losses :: Position a -> Closing a -> Amount 6
    losses pos c =
      let p = c ^. closingLot . price
          b = pos ^. posLot . price
       in case pos ^. posDisp of
            Long -> p - b
            Short -> b - p

    eligibleClose ::
      (Monoid a, Eq a, Show a) =>
      [(Bool, Annotated (Entry a))] ->
      ( Maybe
          ( Zippered
              ( Bool,
                Annotated (Entry a)
              )
          ),
        Map Text (IntMap (Annotated (Entry a)))
      )
    eligibleClose xs = flip runState mempty $
      flip zipperedM xs $ \(w, x) -> do
        poss <- get
        put $ positionsFromEntry poss x
        -- traceM $ "::: eligibleClose.poss =\n" ++ ppShow poss
        -- traceM $ "::: eligibleClose.x =\n" ++ ppShow x
        pure $
          w && Just True == do
            c <- x ^? closing
            pos <-
              poss
                ^? ix (c ^. closingLot . symbol)
                  . ix (c ^. closingIdent)
                  . opening
            guard $ losses pos c < 0
            pure True

    eligibleOpen ::
      (Monoid a, Eq a, Show a) =>
      Text ->
      Int ->
      UTCTime ->
      Bool ->
      (Bool, Annotated (Entry a)) ->
      State (Map Text (IntMap (Annotated (Entry a)))) Bool
    eligibleOpen sym ident anchor inPast (w, y) = do
      poss <- get
      -- traceM $ "::: eligibleOpen.inPast =\n" ++ ppShow inPast
      -- traceM $ "::: eligibleOpen.poss =\n" ++ ppShow poss
      -- traceM $ "::: eligibleOpen.y =\n" ++ ppShow y
      -- unless inPast $
      --   put $ positionsFromEntry poss y
      pure $
        w && Just True == do
          o <- y ^? opening
          guard $ o ^. posLot . symbol == sym
          guard $ o ^. posIdent /= ident
          guard $ not inPast || isJust (poss ^? ix sym . ix (o ^. posIdent))
          guard $ abs (anchor `distance` (y ^. time)) <= 30
          pure True

    handleOpen ::
      (Bool, Annotated (Entry [Washing])) ->
      Bool ->
      Zippered (Bool, Annotated (Entry [Washing])) ->
      Maybe
        ( Zippered (Bool, Annotated (Entry [Washing])),
          (Bool, Annotated (Entry [Washing]))
        )
    handleOpen x inPast part = do
      c <- x ^? _2 . closing
      -- traceM $ "::: handleOpen.c =\n" ++ ppShow c
      y <- part ^? focus
      o <- y ^? _2 . opening
      -- traceM $ "::: handleOpen.o =\n" ++ ppShow o

      -- jww (2021-12-03): The basis adjustment must be washed into the whole
      -- opening position, not a part; so don't use `alignment` here.
      let loss =
            (losses o c * c ^. closingLot . amount) -- the total loss
              / (o ^. posLot . amount)
          o' =
            o & posLot . price -~ loss -- losses increase cost basis
              & posData
                <>~ [ WashedFrom
                        { _washedFromPeriod =
                            if inPast
                              then Future
                              else Past,
                          _washedFromClosingLot = c ^. closingLot,
                          _washedFromCostBasis = o ^. posLot . price
                        }
                    ]
          c' =
            c & closingData
              <>~ [ Wash
                      { _washPeriod =
                          if inPast
                            then Past
                            else Future,
                        _washPositionIdent = o' ^. posIdent,
                        _washCostBasis = o ^. posLot . price
                      }
                  ]
      pure
        ( part & focus .~ (y & _1 .~ False & _2 . opening .~ o'),
          x & _1 .~ False & _2 . closing .~ c'
        )

printWashing :: Washing -> Either TL.Text TL.Text
printWashing = \case
  WashedFrom Past _ x -> Right $ "washed from past " <> printAmount 2 x
  WashedFrom Future _ x -> Right $ "washed from future " <> printAmount 2 x
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
    <|> (keyword "exempt" $> Exempt)
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

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Taxes.USA.WashSaleRule where

import Amount
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Either (fromRight)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Zipper
import GHC.Generics
import Journal.Closings
import Journal.Parse
import Journal.Print
import Journal.Types
import Journal.Utils (distance)
import Text.Show.Pretty hiding (Time)

-- import Debug.Trace

data Period = Past | Future
  deriving (Show, PrettyVal, Eq, Ord, Enum, Bounded, Generic)

makePrisms ''Period

data Washed = Washed
  { _washedPeriod :: !Period,
    _washedPos :: !Int,
    _washedAmount :: !(Amount 6), -- number of shares
    _washedAdjust :: !(Amount 6)
  }
  deriving (Show, PrettyVal, Eq, Ord, Generic)

makeLenses ''Washed

data Washing
  = Exempt
  | WashTo !Washed
  | WashFrom !Washed
  deriving (Show, PrettyVal, Eq, Ord, Generic)

makePrisms ''Washing

instance HasPositionEvent (Const Washing) where
  _Event _ = pure

instance HasLot Washing where
  _Lot _ = pure

-- This implementation of the wash sale rule requires that we know the total
-- set of broker events in advance. Basically, each time we encounter a
-- non-exempt position open, we check 30 days back and then 30 days forward
-- for an eligible losing close; if it exists, the loss is moved into the cost
-- basis of the open.
washSaleRule ::
  [Annotated PositionEvent] ->
  [Annotated [Washing]]
washSaleRule =
  map (fmap (fromRight []) . fst)
    . flip evalState mempty
    . surveyM wash
    . scanPreState
      (\x s -> (Left <$> x, positionsFromEvent s x))
      mempty
  where
    wash ::
      Zipper
        ( Annotated (Either PositionEvent [Washing]),
          Map Text (IntMap (Annotated PositionEvent))
        ) ->
      State
        (IntMap (Amount 6))
        ( Zipper
            ( Annotated (Either PositionEvent [Washing]),
              Map Text (IntMap (Annotated PositionEvent))
            )
        )
    wash z = do
      m <- get
      case go m z of
        Just (res, m') -> do
          put m'
          pure res
        Nothing -> pure z

    go ::
      IntMap (Amount 6) ->
      Zipper
        ( Annotated (Either PositionEvent [Washing]),
          Map Text (IntMap (Annotated PositionEvent))
        ) ->
      Maybe
        ( Zipper
            ( Annotated (Either PositionEvent [Washing]),
              Map Text (IntMap (Annotated PositionEvent))
            ),
          IntMap (Amount 6)
        )
    go m z = do
      -- Washing begins by looking for closing events that closed at a loss.
      -- Note that "closing at a loss" might be affected by previous washings,
      -- so we use the 'survey' method to scan through the list of entries, so
      -- that each time this function is called, we take any past washings
      -- also into account.
      let x = focus z
      c <- x ^? _1 . item . _Left . _Close
      -- traceM $ "c = " ++ ppShow c

      -- Every valid closing must have a corresponding open position.
      let sym = c ^. closingLot . symbol
          ident = c ^. closingPos
      pos <- x ^? _2 . ix sym . ix ident . item . _Open

      -- Ignore closing that made a profit.
      let loss =
            let p = c ^. closingLot . price
                b = pos ^. posLot . price
             in case pos ^. posDisp of
                  Long -> p - b
                  Short -> b - p
      guard $ loss < 0

      -- When looking for openings, we have to take this closing into account
      -- in the map of open positions.
      let poss' :: Map Text (IntMap (Annotated PositionEvent)) = case x of
            (ev, m') -> case ev ^? item . _Left of
              Just pe -> positionsFromEvent m' (ev & item .~ pe)
              Nothing -> mempty

      let mres = mapLeftThenRightUntils z $ \inLeft y -> do
            o <- y ^? _1 . item . _Left . _Open

            guard $ o ^. posLot . symbol == sym
            guard $ o ^. posIdent /= ident
            guard $ not inLeft || isJust (poss' ^? ix sym . ix (o ^. posIdent))
            guard $ abs ((x ^. _1 . time) `distance` (y ^. _1 . time)) <= 30

            let inOpen = o ^. posLot . amount
                inClose = c ^. closingLot . amount
                washed = fromMaybe 0 (m ^? ix (o ^. posIdent))
            guard $ inOpen - washed >= inClose

            -- traceM $ "o = " ++ ppShow o
            -- traceM $ "m = " ++ ppShow m
            pure
              ( [ y,
                  y
                    & _1 . item
                      .~ Right
                        [ WashFrom
                            Washed
                              { _washedPeriod =
                                  if inLeft
                                    then Future
                                    else Past,
                                _washedPos = c ^. closingPos,
                                _washedAmount = inClose,
                                _washedAdjust = (loss * inClose) / inOpen
                              }
                        ]
                ],
                ( m & at (o ^. posIdent) ?~ washed + inClose,
                  [ x,
                    x
                      & _1 . item
                        .~ Right
                          [ WashTo
                              Washed
                                { _washedPeriod =
                                    if inLeft
                                      then Past
                                      else Future,
                                  _washedPos = o ^. posIdent,
                                  _washedAmount = inOpen,
                                  _washedAdjust = loss
                                }
                          ]
                  ]
                )
              )

      case mres of
        Just (z', (m', outers)) -> do
          z'' <- overlay z' outers
          pure (z'', m')
        Nothing -> pure (z, m)

instance Printable Washing where
  printItem = either id id . printWashing

printWashing :: Washing -> Either TL.Text TL.Text
printWashing = \case
  WashTo _ -> error "NYI: printWashing.WashTo"
  WashFrom _ -> error "NYI: printWashing.WashFrom"
  Exempt -> Left "exempt"

parseWashing :: Parser Washing
parseWashing =
  Exempt <$ keyword "exempt"

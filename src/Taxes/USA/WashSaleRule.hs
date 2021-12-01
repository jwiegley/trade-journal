{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Taxes.USA.WashSaleRule (washSaleRule) where

import Amount
import Control.Exception (assert)
import Control.Lens
import Control.Monad
import Data.Time
import Debug.Trace
import GHC.Generics hiding (to)
import Journal.Split
import Journal.Types
import Journal.Utils (distance)
import Journal.Zippered
import Text.Show.Pretty hiding (Time)

data Washing
  = Exempted
  | Eligible
  | NotApplicable
  | WashFromPast (Amount 6)
  | WashFromFuture (Amount 6)
  | WashedBackward
  | WashedFoward
  deriving
    ( Show,
      PrettyVal,
      Eq,
      Ord,
      Generic
    )

makePrisms ''Washing

opening :: Traversal' (Washing, Annotated Entry) Position
opening = _2 . item . _Event . _Open

closing :: Traversal' (Washing, Annotated Entry) Closing
closing = _2 . item . _Event . _Close

-- This implementation of the wash sale rule requires that we know the total
-- set of broker events in advance. Basically, each time we encounter a
-- non-exempt position open, we check 30 days back and then 30 days forward
-- for an eligible losing close; if it exists, the loss is moved into the cost
-- basis of the open.
washSaleRule ::
  [Annotated Entry] ->
  [(Washing, Annotated Entry)]
washSaleRule =
  go
    . map
      ( \x ->
          ( if null (x ^.. details . traverse . _Exempt)
              then Eligible
              else Exempted,
            x
          )
      )
  where
    go :: [(Washing, Annotated Entry)] -> [(Washing, Annotated Entry)]
    go xs = maybe xs go $ do
      -- The wash sale rule proceeds by looking for losing sales that haven't
      -- been washed. If there are none, we are done.
      let z = eligibleClose xs
      x <- z ^? focus
      c <- x ^? closing
      traceM $ "c = " ++ ppShow c

      -- Once an eligible losing sale is found, we look for an eligible
      -- opening within 30 days before or after that sale.
      (z', xx') <-
        applyToPrefixOrSuffix (eligibleOpen (x ^. _2 . time)) (handleOpen x) z

      let z'' = z' & focii .~ xx'
      guard $ z /= z''
      pure $ unzippered z''

    gains :: Closing -> Amount 6
    gains c =
      let p = c ^. closingLot . price
          b = c ^. closingPos . posBasis
       in case c ^. closingPos . posDisp of
            Long -> p - b
            Short -> b - p

    eligibleClose ::
      [(Washing, Annotated Entry)] -> Zippered (Washing, Annotated Entry)
    eligibleClose = zippered $ \(w, x) ->
      w == Eligible
        && gains (x ^?! item . _Event . _Close) < 0

    eligibleOpen anchor (w, y) =
      w == Eligible
        && abs (anchor `distance` (y ^. time)) <= 30

    handleOpen ::
      (Washing, Annotated Entry) ->
      Bool ->
      Zippered (Washing, Annotated Entry) ->
      Maybe
        ( Zippered (Washing, Annotated Entry),
          [(Washing, Annotated Entry)]
        )
    handleOpen x _inPast part = do
      c <- x ^? closing
      let loss = gains c
      assert (loss < 0) $ do
        y <- part ^? focus
        o <- y ^? opening
        traceM $ "o = " ++ ppShow o
        traceM $ "loss = " ++ ppShow loss
        (Just (l, r), reyz) <-
          alignedA
            (c ^. closingLot)
            (o ^. posLot)
            (curry pure)
            pure
            pure
        let o' =
              o & posBasis .~ ((o ^. posBasis) - loss)
                & posLot .~ r
        pure
          ( part & focii
              .~ ( ( y & _1 .~ NotApplicable
                       & opening .~ o'
                   ) :
                   case reyz of
                     Remainder (Right z) ->
                       [ y & _1 .~ Exempted
                           & opening . posLot .~ z
                       ]
                     _ -> []
                 ),
            ( x & _1 .~ NotApplicable
                & closing . closingPos .~ o'
                & closing . closingLot .~ l
            ) :
            case reyz of
              Remainder (Left z) ->
                [x & closing . closingLot .~ z]
              _ -> []
          )

testWashSale :: IO ()
testWashSale = do
  now <- getCurrentTime
  putStrLn $
    dumpStr $
      washSaleRule (map (\x -> Annotated x now []) dataset)
  where
    dataset :: [Entry]
    dataset =
      [ Event
          ( Open
              ( Position
                  1
                  Lot
                    { _amount = 100,
                      _symbol = "FOO",
                      _price = 100
                    }
                  Long
                  100
              )
          ),
        Event
          ( Open
              ( Position
                  2
                  Lot
                    { _amount = 200,
                      _symbol = "FOO",
                      _price = 200
                    }
                  Long
                  200
              )
          ),
        Event
          ( Open
              ( Position
                  3
                  Lot
                    { _amount = 300,
                      _symbol = "FOO",
                      _price = 300
                    }
                  Long
                  300
              )
          ),
        Event
          ( Close
              ( Closing
                  ( Position
                      1
                      Lot
                        { _amount = 100,
                          _symbol = "FOO",
                          _price = 100
                        }
                      Long
                      100
                  )
                  Lot
                    { _amount = 100,
                      _symbol = "FOO",
                      _price = 50
                    }
              )
          ),
        Event
          ( Open
              ( Position
                  4
                  Lot
                    { _amount = 200,
                      _symbol = "FOO",
                      _price = 200
                    }
                  Long
                  200
              )
          ),
        Event
          ( Open
              ( Position
                  5
                  Lot
                    { _amount = 300,
                      _symbol = "FOO",
                      _price = 300
                    }
                  Long
                  300
              )
          ),
        Event
          ( Close
              ( Closing
                  ( Position
                      2
                      Lot
                        { _amount = 200,
                          _symbol = "FOO",
                          _price = 200
                        }
                      Long
                      200
                  )
                  Lot
                    { _amount = 300,
                      _symbol = "FOO",
                      _price = 300
                    }
              )
          )
      ]

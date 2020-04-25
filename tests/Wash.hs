{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Wash (testWashSaleRule) where

import Control.Lens
import Control.Monad.Morph
import Control.Monad.Trans.State
import Pos
import Test.Tasty
import Test.Tasty.HUnit
import ThinkOrSwim.Transaction
import ThinkOrSwim.Wash

testWashSaleRule :: TestTree
testWashSaleRule = testGroup "washSaleRule"
    [ testCase "open" $
      flip evalStateT [] $ do
          let aapl0215 = 12@@300 ## "2020-02-15" $$ 0.00
          wash aapl0215 @?== ( [ aapl0215 ]
                             , [ aapl0215 ] )

    , testCase "open, open <30" $
      flip evalStateT [] $ do
          let aapl0215 = 12@@300 ## "2020-02-15" $$ 0.00
              aapl0216 = 12@@300 ## "2020-02-16" $$ 0.00
          wash aapl0215
          wash aapl0216 @?== ( [ aapl0215
                               , aapl0216 ]
                             , [ aapl0216 ] )

    , testCase "open, open >30" $
      flip evalStateT [] $ do
          let aapl0215 = 12@@300 ## "2020-02-15" $$ 0.00
              aapl0416 = 12@@300 ## "2020-04-16" $$ 0.00
          wash aapl0215
          wash aapl0416 @?== ( [ aapl0416 ]
                             , [ aapl0416 ] )

    , testCase "open, sell at loss <30" $
      flip evalStateT [] $ do
          let aapl0215 =    12@@300 ## "2020-02-15" $$    0.00
              aapl0216 = (-12)@@200 ## "2020-02-16" $$ 1200.00
          wash aapl0215
          wash aapl0216 @?== ( [ aapl0216 ]
                             , [ aapl0216 ] )

    , testCase "open, sell at gain" $
      flip evalStateT [] $ do
          let aapl0215 =    12@@300 ## "2020-02-15" $$      0.00
              aapl0216 = (-12)@@400 ## "2020-02-16" $$ (-1200.00)
          wash aapl0215
          wash aapl0216 @?== ( [ aapl0215 ]
                             , [ aapl0216 ] )

    , testCase "open, sell at loss >30" $
      flip evalStateT [] $ do
          let aapl0215 =    12@@300 ## "2020-02-15" $$    0.00
              aapl0416 = (-12)@@200 ## "2020-04-16" $$ 1200.00
          wash aapl0215
          wash aapl0416 @?== ([], [aapl0416])

    , testCase "open, sell at loss <30, open again <30" $
      flip evalStateT [] $ do
          let aapl0215o =    12@@300 ## "2020-02-15" $$     0.00
              aapl0216c = (-12)@@290 ## "2020-02-16" $$   120.00
              aapl0217o =    12@@300 ## "2020-02-17" $$     0.00

          wash aapl0215o @?== ( [ aapl0215o ]
                              , [ aapl0215o ] )
          wash aapl0216c @?== ( [ aapl0216c ]
                              , [ aapl0216c ] )
          wash aapl0217o @?==
              ( [ 12@@420 ## "2020-02-17" $$ 0.00 ]
              , [ 12@@420 ## "2020-02-17" $$ (-120.00) ] )

    , testCase "open, open, sell at loss <30" $
      flip evalStateT [] $ do
          let aapl0215o =    12@@300 ## "2020-02-15" $$   0.00
              aapl0216o =     5@@155 ## "2020-02-16" $$   0.00
              aapl0217o =     5@@155 ## "2020-02-17" $$   0.00
              aapl0218c = (-12)@@290 ## "2020-02-18" $$ 120.00

          wash aapl0215o @?== ( [ aapl0215o ]
                              , [ aapl0215o ] )
          wash aapl0216o @?== ( [ aapl0215o
                                , aapl0216o ]
                              , [ aapl0216o ] )
          wash aapl0217o @?== ( [ aapl0215o
                                , aapl0216o
                                , aapl0217o ]
                              , [ aapl0217o ] )
          wash aapl0218c @?==
              ( [ (-2)@@48.3333 ## "2020-02-18" $$ 20.00
                , 5@@205 ## "2020-02-18" $$ 0.00
                , 5@@205 ## "2020-02-18" $$ 0.00 ]
              , [ aapl0218c
                , aapl0216o & loss .~ 0.00
                            & quantity %~ negate
                , 5@@205 ## "2020-02-18" $$ (-50.00)
                , aapl0217o & loss .~ 0.00
                            & quantity %~ negate
                , 5@@205 ## "2020-02-18" $$ (-50.00) ] )

    , testCase "open many, sell many" $
      flip evalStateT [] $ do
          wash $  140.00  @@ (140.00 *  99.7792) ## "2019-06-24" $$ 0.00
          wash $   10.00  @@ ( 10.00 *  89.785)  ## "2019-06-24" $$ 0.00
          wash $   30.00  @@ ( 30.00 * 106.68)   ## "2019-06-24" $$ 0.00
          wash $  170.00  @@ (170.00 *  85.8415) ## "2019-06-25" $$ 0.00

          wash $   50.00  @@ ( 50.00 *  85.80)   ## "2019-07-01" $$ 0.00

          wash  ((-50.00) @@ (50.00 *  99.7792)  ## "2019-07-03" $$ 655.31)
              @?== ( [   90.00  @@  8980.128 ## "2019-06-24" $$ 0.00
                     ,   10.00  @@   897.85  ## "2019-06-24" $$ 0.00
                     ,   30.00  @@  3200.40  ## "2019-06-24" $$ 0.00
                     ,  170.00  @@ 14593.055 ## "2019-06-25" $$ 0.00
                     ,   50.00  @@  4290.00  ## "2019-07-01" $$ 0.00
                     , (-50.00) @@  4988.96  ## "2019-07-03" $$ 0.00
                     ,   50.00  @@  5644.27  ## "2019-07-03" $$ 0.00
                     ]
                   , [ (-50.00) @@ (50.00 *  99.7792)  ## "2019-07-03" $$ 655.31
                     , (-50.00) @@ (50.00 *  99.7792)  ## "2019-06-24" $$ 0.00
                     ,   50.00  @@ (50.00 * 112.8854)  ## "2019-07-03" $$ (-655.31)
                     ]
                   )

          wash  ((-90.00) @@ (90.00 *  99.7792)  ## "2019-07-03" $$ 1170.42)
              @?== ( []
                   , [ (-90.00) @@ (50.00 *  99.7792)  ## "2019-06-24" $$ 1170.42
                     , (-50.00) @@ (50.00 *  99.7792)  ## "2019-06-24" $$ 0.00
                     ,   50.00  @@ (50.00 * 112.8854)  ## "2019-07-02" $$ (-650.23)
                     , (-40.00) @@ (50.00 *  99.7792)  ## "2019-06-24" $$ 0.00
                     ,   40.00  @@ (50.00 * 112.8854)  ## "2019-07-29" $$ (-520.19)
                     ]
                   )

          wash  ((-10.00) @@ (90.00 *  89.785)  ## "2019-07-03" $$ 30.10)
              @?== ( []
                   , [ (-10.00) @@ (50.00 *  89.785)   ## "2019-06-24" $$ 30.10
                     , (-10.00) @@ (50.00 *  89.785)   ## "2019-06-24" $$ 0.00
                     ,   10.00  @@ (50.00 *  92.795)   ## "2019-07-29" $$ (-30.10)
                     ]
                   )
    ]

wash :: Transactional a => a -> StateT [a] IO ([a] , [a])
wash pl = hoist (pure . runIdentity) $ do
    res <- washSaleRule day pl
    events <- get
    pure (events, res)

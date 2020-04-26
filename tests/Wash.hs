{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Wash (testWashSaleRule) where

import Control.Lens
import Control.Monad.Morph
import Control.Monad.Trans.State
import Debug.Trace (traceM)
import Pos
import Test.Tasty
import Test.Tasty.HUnit
import Text.PrettyPrint hiding (($$))
import ThinkOrSwim.Transaction
import ThinkOrSwim.Wash

testWashSaleRule :: TestTree
testWashSaleRule = testGroup "washSaleRule"
    [
    --   testCase "open" $
    --   flip evalStateT [] $ do
    --       let aapl0215 = 12@@@300 ## "2020-02-15" $$ 0.00
    --       wash aapl0215 @?== ( [ aapl0215 ]
    --                          , [ aapl0215 ] )

    -- , testCase "open, open <30" $
    --   flip evalStateT [] $ do
    --       let aapl0215 = 12@@@300 ## "2020-02-15" $$ 0.00
    --           aapl0216 = 12@@@300 ## "2020-02-16" $$ 0.00
    --       wash aapl0215
    --       wash aapl0216 @?== ( [ aapl0215
    --                            , aapl0216 ]
    --                          , [ aapl0216 ] )

    -- , testCase "open, open >30" $
    --   flip evalStateT [] $ do
    --       let aapl0215 = 12@@@300 ## "2020-02-15" $$ 0.00
    --           aapl0416 = 12@@@300 ## "2020-04-16" $$ 0.00
    --       wash aapl0215
    --       wash aapl0416 @?== ( [ aapl0416 ]
    --                          , [ aapl0416 ] )

    -- , testCase "open, sell at loss <30" $
    --   flip evalStateT [] $ do
    --       let aapl0215 =    12@@@300 ## "2020-02-15" $$    0.00
    --           aapl0216 = (-12)@@@200 ## "2020-02-16" $$ 1200.00
    --       wash aapl0215
    --       wash aapl0216 @?== ( [ aapl0216 ]
    --                          , [ aapl0216 ] )

    -- , testCase "open, sell at gain" $
    --   flip evalStateT [] $ do
    --       let aapl0215 =    12@@@300 ## "2020-02-15" $$      0.00
    --           aapl0216 = (-12)@@@400 ## "2020-02-16" $$ (-1200.00)
    --       wash aapl0215
    --       wash aapl0216 @?== ( [ aapl0215 ]
    --                          , [ aapl0216 ] )

    -- , testCase "open, sell at loss >30" $
    --   flip evalStateT [] $ do
    --       let aapl0215 =    12@@@300 ## "2020-02-15" $$    0.00
    --           aapl0416 = (-12)@@@200 ## "2020-04-16" $$ 1200.00
    --       wash aapl0215
    --       wash aapl0416 @?== ([], [aapl0416])

    -- , testCase "open, sell at loss <30, open again <30" $
    --   flip evalStateT [] $ do
    --       let aapl0215o =    12@@@300 ## "2020-02-15" $$     0.00
    --           aapl0216c = (-12)@@@290 ## "2020-02-16" $$   120.00
    --           aapl0217o =    12@@@300 ## "2020-02-17" $$     0.00

    --       wash aapl0215o @?== ( [ aapl0215o ]
    --                           , [ aapl0215o ] )
    --       wash aapl0216c @?== ( [ aapl0216c ]
    --                           , [ aapl0216c ] )
    --       wash aapl0217o @?==
    --           ( [ 12@@@420 ## "2020-02-17" $$ 0.00 ]
    --           , [ 12@@@420 ## "2020-02-17" $$ (-120.00) ] )

    -- , testCase "open, open, sell at loss <30" $
    --   flip evalStateT [] $ do
    --       let aapl0215o =    12@@@300 ## "2020-02-15" $$   0.00
    --           aapl0216o =     5@@@155 ## "2020-02-16" $$   0.00
    --           aapl0217o =     5@@@155 ## "2020-02-17" $$   0.00
    --           aapl0218c = (-12)@@@290 ## "2020-02-18" $$ 120.00

    --       wash aapl0215o @?== ( [ aapl0215o ]
    --                           , [ aapl0215o ] )
    --       wash aapl0216o @?== ( [ aapl0215o
    --                             , aapl0216o ]
    --                           , [ aapl0216o ] )
    --       wash aapl0217o @?== ( [ aapl0215o
    --                             , aapl0216o
    --                             , aapl0217o ]
    --                           , [ aapl0217o ] )
    --       wash aapl0218c @?==
    --           ( [ (-2)@@@48.3333 ## "2020-02-18" $$ 20.00
    --             , 5@@@205 ## "2020-02-18" $$ 0.00
    --             , 5@@@205 ## "2020-02-18" $$ 0.00 ]
    --           , [ aapl0218c
    --             , aapl0216o & loss .~ 0.00
    --                         & quantity %~ negate
    --             , 5@@@205 ## "2020-02-18" $$ (-50.00)
    --             , aapl0217o & loss .~ 0.00
    --                         & quantity %~ negate
    --             , 5@@@205 ## "2020-02-18" $$ (-50.00) ] )

      testCase "open many, sell many" $
      flip evalStateT [] $ do
          -- Establish pre-existing equity
          wash $  140 @@  99.7792 ## "2019-06-24" $$ 0.00 & old .~ True
          wash $   10 @@  89.785  ## "2019-06-24" $$ 0.00 & old .~ True
          wash $   30 @@ 106.68   ## "2019-06-24" $$ 0.00 & old .~ True
          wash $  170 @@  85.8415 ## "2019-06-25" $$ 0.00 & old .~ True

          wash (   50 @@  85.80   ## "2019-07-01" $$ 0.00 ) @?==
              ( [ 140 @@  99.7792 ## "2019-06-24" $$ 0.00
                ,  10 @@  89.785  ## "2019-06-24" $$ 0.00
                ,  30 @@ 106.68   ## "2019-06-24" $$ 0.00
                , 170 @@  85.8415 ## "2019-06-25" $$ 0.00
                ,  50 @@  85.80   ## "2019-07-01" $$ 0.00
                ]
              , [  50 @@  85.80   ## "2019-07-01" $$ 0.00
                ]
              )

          -- | Wash Sale Adj | [140] | 06/26/19 | 1399.19 |
          -- | Wash Sale Adj | [30]  | 06/26/19 |  351.86 |

          -- | Wash Sale Adj | [50]  | 07/01/19 | 655.31 |
          wash (  (-50) @@  99.7792 ## "2019-06-24" $$   655.31 )

          wash (    50  @@  85.50   ## "2019-07-02" $$     0.00 )

          -- | Wash Sale Adj | [50]  | 07/03/19 | 650.23 |
          -- | Wash Sale Adj | [40]  | 07/03/19 | 520.19 |
          wash (  (-90) @@  99.7792 ## "2019-06-24" $$  1170.42 )

          -- | Wash Sale Adj | [10]  | 07/03/19 |  30.10 |
          wash (  (-10) @@  89.785  ## "2019-06-24" $$   30.10 )

          -- | Wash Sale Adj | [30]  | 07/03/19 | 556.26 |
          wash $  (-30) @@ 106.68   ## "2019-06-24" $$   556.26
          wash $  (-70) @@  85.8415 ## "2019-06-25" $$ (-160.76)
          wash $ (-100) @@  85.8415 ## "2019-06-25" $$ (-824.15)
          -- | Wash Sale Adj | [50]  | 07/12/19 | 241.17 |
          wash $  (-50) @@  98.5046 ## "2019-07-02" $$   241.17
          -- | Wash Sale Adj | [50]  | 07/12/19 | 221.08 |
          wash $  (-50) @@  98.9062 ## "2019-07-01" $$   221.08
          wash $   400  @@  95.7852 ## "2019-07-29" $$     0.00
          wash $   100  @@  95.7852 ## "2019-07-29" $$     0.00
          -- | Wash Sale Adj | [200] | 07/29/19 |  26.06 |
          wash $ (-400) @@  98.5516 ## "2019-07-29" $$  1158.67
          wash $ (-100) @@ 100.4077 ## "2019-07-29" $$   422.57
          wash $   200  @@  96.1303 ## "2019-07-30" $$     0.00
          wash $   100  @@  86.02   ## "2019-09-06" $$     0.00
          wash $ (-100) @@  86.02   ## "2019-09-06" $$ (-898.00)
          wash $ (-200) @@  96.1303 ## "2019-07-30" $$   226.73

          pure ()
    ]

wash :: Transactional a => a -> StateT [a] IO ([a] , [a])
wash pl = hoist (pure . runIdentity) $ do
    (doc, res) <- washSaleRule day pl
    traceM $ render doc
    events <- get
    pure (events, res)

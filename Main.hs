module Main where

import Data.Text as T
import Network.HTTP.Client
import Servant.Client
import System.Environment
import ThinkOrSwim.API

main :: IO ()
main = do
  accountId:accessToken:_ <- Prelude.map T.pack <$> getArgs
  mgr <- newManager defaultManagerSettings
  let call = getTransactions
          (AccountId accountId)
          Nothing
          Nothing
          Nothing
          Nothing
          (Just (AccessToken accessToken))
  res <- runClientM call $ mkClientEnv mgr $ BaseUrl
      { baseUrlScheme = Https
      , baseUrlHost = "api.tdameritrade.com"
      , baseUrlPort = 443
      , baseUrlPath = "v1"
      }
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right xs -> print (Prelude.head xs)

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Wow.Twitter.FilteredStream where

import Control.Monad.IO.Class (MonadIO)
import Prelude hiding (filter)
import Data.Aeson (decodeStrict)

import Control.Monad (void)
import Configuration.Dotenv ( loadFile, defaultConfig )
import System.Environment (getEnvironment)
import Wow.Twitter.Types (StreamEntry, tokenFromEnvPoly)
import Polysemy (Sem, Member)
import Wow.Effects.HttpLongPolling (HttpLongPolling, Request (Request), url, headers)
import Wow.Effects.Env (Env)
import qualified Wow.Effects.HttpLongPolling as HLP

showEnv :: IO ()
showEnv = do
  print =<< getEnvironment

loadDotEnv :: (MonadIO m) => m ()
loadDotEnv = void $ loadFile defaultConfig

filteredStream :: forall r . ( Member Env r, Member HttpLongPolling r) =>(StreamEntry -> Sem r ()) -> Sem r ()
filteredStream handler' = do
  let
    handler = handler1 . decodeStrict
      where
        handler1 = \case
          Just x -> handler' x
          Nothing -> pure ()
  token <- tokenFromEnvPoly
  let request = Request {
    HLP.url = "https://api.twitter.com/2/tweets/sample/stream",
    HLP.headers = [("Authorization", "Bearer " <> token)],
    HLP.method = "GET"
  }
  HLP.httpLongPolling request handler

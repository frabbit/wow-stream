{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Redundant lambda" #-}
module Wow.Twitter.FilteredStream where

import Conduit (ConduitT, runConduit, (.|), sinkNull, mapC)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString as BS
import Network.HTTP.Client.Conduit
  ( Response (responseBody),
    method,
    newManager,
    parseRequest,
    requestHeaders,
    responseTimeout,
    withResponse, responseTimeoutMicro,
  )
import UnliftIO (MonadUnliftIO, askRunInIO)
import Prelude hiding (filter)
import Data.Aeson (decodeStrict)
import Data.Conduit.Combinators (iterM, filter)

import Control.Monad (void)
import Configuration.Dotenv ( loadFile, defaultConfig )
import System.Environment (getEnvironment)
import Data.Maybe (fromJust, isJust)
import Wow.Twitter.Types (StreamEntry, Env (..), AppT, runAppT, tokenFromEnv)
import Polysemy (Sem, Member, Embed)
import Wow.Effects.HttpLongPolling (HttpLongPolling, Request (Request), url, headers)
import qualified Wow.Effects.HttpLongPolling as HLP

newtype MyIO a = MyIO {unMyIO :: IO a} deriving (Monad, Functor, MonadIO, Applicative, MonadUnliftIO)

showEnv :: IO ()
showEnv = do
  print =<< getEnvironment

loadDotEnv :: _ => _ ()
loadDotEnv = void $ loadFile defaultConfig

filteredStream :: forall r . ( Member HttpLongPolling r, Member (Embed IO) r) =>(StreamEntry -> Sem r ()) -> Sem r ()
filteredStream handler' = do
  let
    handler = handler1 . decodeStrict
      where
        handler1 = \case
          Just x -> handler' x
          Nothing -> pure ()
  token <- tokenFromEnv
  let request = Request {
    HLP.url = "https://api.twitter.com/2/tweets/search/stream",
    HLP.headers = [("Authorization", "Bearer " <> token)],
    HLP.method = "GET"
  }
  HLP.httpLongPolling request handler
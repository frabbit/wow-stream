{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE BlockArguments #-}
{-# HLINT ignore "Redundant lambda" #-}

module Wow.Effects.HttpLongPolling where

import Prelude

import Network.HTTP.Client.Conduit
  ( Response (responseBody),
    method,
    newManager,
    parseRequest,
    requestHeaders,
    responseTimeout,
    withResponse, responseTimeoutMicro,
  )

import Wow.Twitter.Types
    ( tokenFromEnv,
      runAppT,
      Env(Env, manager),
      AppT,
      StreamEntry,
      loadDotEnv )

import Polysemy ( makeSem, Sem, Member, Embed, embed, interpretH, pureT, bindT, getInitialStateT, embedToFinal, Final, runFinal )

import Data.Text (Text)
import qualified Data.ByteString as BS
import Data.Kind (Type)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Conduit (ConduitT, runConduit, (.|), sinkNull)
import Data.Conduit.Combinators (iterM)
import Control.Monad (void)
import qualified Data.Text as T
import Network.HTTP.Types (Method)
import Data.CaseInsensitive (CI)
import Control.Lens.Operators ((&))
import Data.Aeson (decodeStrict)

data Request = Request {
  url :: Text,
  method :: Method,
  headers :: [(CI BS.ByteString, BS.ByteString)]
}

data HttpLongPolling (m::Type -> Type) a where
  HttpLongPolling :: Request -> (BS.ByteString -> m ()) -> HttpLongPolling m ()

makeSem ''HttpLongPolling

pollingApp :: Sem (HttpLongPolling ': Embed IO ': Final IO ':  r) ()
pollingApp = do
  httpLongPolling Request { url = "https://api.twitter.com/2/tweets/search/stream", method = "GET", headers = []} $ \s -> do
    let (x::Maybe StreamEntry) = decodeStrict s
    embed $ case x of
      Just a -> print a.tweet.text
      Nothing -> pure ()

appToIo :: Sem (HttpLongPolling ': Embed IO  ': Final IO ': '[]) a -> IO a
appToIo p = p
  & httpLongPollingToIO appToIo
  & embedToFinal
  & runFinal

main :: IO ()
main = do
  loadDotEnv
  pollingApp & appToIo

httpLongPollingToIO :: forall r a . (Member (Embed IO) r) => (forall x . Sem (HttpLongPolling ': r) x -> IO x) -> Sem (HttpLongPolling ': r) a -> Sem r a
httpLongPollingToIO nt = interpretH $ \case
  HttpLongPolling request callback -> do
    callback0 <- bindT callback
    is <- getInitialStateT
    let cb = \bs -> void . nt . callback0 $ (bs <$ is)
    embed (execHttpLongPolling request cb) >>= pureT

execHttpLongPolling :: () => Request -> (BS.ByteString -> IO ()) -> IO ()
execHttpLongPolling request cb = do
  manager <- newManager :: IO _
  let env = Env { manager }
  runAppT env $ do
    req <- liftIO mkReq  :: AppT IO _
    withResponse req handleResponse :: AppT IO _
  where
    mkReq = do
      initReq <- parseRequest $ T.unpack request.url
      token <- tokenFromEnv
      let r =
            initReq
              { method = request.method,
                requestHeaders = requestHeaders initReq <> request.headers <> [("Authorization", "Bearer " <> token)],
                responseTimeout = responseTimeoutMicro (20 * 1000000) -- one sec
              }
      pure r
    handleResponse :: (MonadIO w) => Response (ConduitT () BS.ByteString IO ()) -> AppT w ()
    handleResponse = liftIO . withBody . responseBody
      where
        withBody :: ConduitT _ BS.ByteString IO () -> IO ()
        withBody body = runConduit ( body .| iterM cb .| sinkNull )

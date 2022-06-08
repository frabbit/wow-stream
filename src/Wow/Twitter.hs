{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE RecordWildCards #-}
module Wow.Twitter where

import Conduit (ConduitT, runConduit, (.|), sinkNull, mapC)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT (ReaderT, runReaderT), MonadPlus (mzero))
import qualified Data.ByteString as BS
import Network.HTTP.Client.Conduit
  ( HasHttpManager (getHttpManager),
    Manager,
    Response (responseBody),
    method,
    newManager,
    parseRequest,
    requestHeaders,
    requestBody,
    responseTimeout,
    withResponse, responseTimeoutMicro, RequestBody (RequestBodyLBS),
  )
import UnliftIO (MonadUnliftIO)
import Prelude hiding (filter)
import Network.HTTP.Conduit (httpLbs)
import Data.Aeson (Value(Object, Array), FromJSON (parseJSON), (.:), decode, encode, ToJSON (toJSON), object, decodeStrict)
import Data.Text (Text)
import Data.Conduit.Combinators (iterM, filter)

import Control.Monad (void)
import Configuration.Dotenv ( loadFile, defaultConfig )
import System.Environment (getEnv, getEnvironment)
import qualified Data.Text.Encoding as BS
import qualified Data.Text as Text
import Data.Maybe (fromJust, isJust)

newtype Env = Env
  { manager :: Manager
  }

newtype AppT m a = AppT {unAppT :: ReaderT Env m a} deriving (Monad, MonadReader Env, Functor, MonadIO, Applicative, MonadUnliftIO, MonadThrow)

runAppT :: Env -> AppT m a -> m a
runAppT env = flip runReaderT env . unAppT

instance HasHttpManager Env where
  getHttpManager = (.manager)

tokenFromEnv :: (MonadIO m) => m BS.ByteString
tokenFromEnv = BS.encodeUtf8 . Text.pack <$> liftIO (getEnv "TWITTER_BEARER_TOKEN")

data Rule = Rule {
  ruleId :: Text
} deriving (Show)

instance FromJSON Rule where
  parseJSON (Object o) =
    Rule <$> o .: "id"
  parseJSON _ = mzero

data Rules = Rules {
  rules :: [Rule]
} deriving (Show)

instance FromJSON Rules where
  parseJSON o@(Array _) = do
    rules <- parseJSON o
    pure $ Rules { rules }
  parseJSON _ = mzero

data GetRulesResponse = GetRulesResponse Rules deriving (Show)

instance FromJSON GetRulesResponse where
  parseJSON (Object o) = do
    d <- o .: "data"
    r <- parseJSON d
    pure $ GetRulesResponse r
  parseJSON _ = mzero

getRules :: IO (Maybe GetRulesResponse)
getRules = do
  req <- mkReq
  manager <- newManager
  resp <- httpLbs req manager
  let body = responseBody resp
  print body
  pure $ decode body
  where
    mkReq = do
      initReq <- parseRequest "https://api.twitter.com/2/tweets/search/stream/rules"
      token <- tokenFromEnv
      let
        r = initReq
          { method = "GET",
            requestHeaders = requestHeaders initReq <> [("Authorization", "Bearer " <> token)]
          }
      pure r


type RuleId = Text

deleteRules :: [RuleId] -> IO ()
deleteRules ids = do
  req <- mkReq
  manager <- newManager
  res <- httpLbs req manager
  print res
  pure ()
  where
    mkReq = do
      initReq <- parseRequest "https://api.twitter.com/2/tweets/search/stream/rules"
      token <- tokenFromEnv
      let
        r = initReq
          { method = "POST",
            requestHeaders = requestHeaders initReq <> [("Authorization", "Bearer " <> token), ("Content-type","application/json")],
            requestBody = RequestBodyLBS $ encode PostRules { delete = Just ids, add = Nothing }
          }
      pure r

data RuleDef = RuleDef String deriving (Show)

instance ToJSON RuleDef where
  toJSON (RuleDef s) = object [("value", toJSON s)]

data PostRules = PostRules {
  add :: Maybe [RuleDef],
  delete :: Maybe [RuleId]
} deriving (Show)

data MatchingRule = MatchingRule {
  matchingRuleId :: RuleId,
  tag :: Text
} deriving (Show)

instance FromJSON MatchingRule where
  parseJSON (Object o) = do
    MatchingRule <$> o .: "id" <*> o .: "tag"
  parseJSON _ = mzero

data Tweet = Tweet {
  tweetId :: Text,
  text :: Text
} deriving (Show)

instance FromJSON Tweet where
  parseJSON (Object o) = do
    Tweet <$> o .: "id" <*> o .: "text"
  parseJSON _ = mzero

data StreamEntry = StreamEntry {
  tweet :: Tweet,
  matchingRules :: [MatchingRule]
} deriving (Show)

instance FromJSON StreamEntry where
  parseJSON (Object o) = do
    tweet <- parseJSON =<< (o .: "data")
    matchingRules <- parseJSON =<< (o .: "matching_rules")
    pure $ StreamEntry { .. }
  parseJSON _ = mzero

instance ToJSON PostRules where
  toJSON pr = do
    let
      adds = case pr.add of
        Just r -> [("add", toJSON r)]
        Nothing -> []
      deletes = case pr.delete of
        Just ids -> [("delete", object [("ids", toJSON ids)] )]
        Nothing -> []
    object (adds <> deletes)

addRule :: RuleDef -> IO ()
addRule rule = do
  req <- mkReq
  manager <- newManager
  res <- httpLbs req manager
  print res
  pure ()
  where
    mkReq = do
      initReq <- parseRequest "https://api.twitter.com/2/tweets/search/stream/rules"
      token <- tokenFromEnv
      let
        r = initReq
          { method = "POST",
            requestHeaders = requestHeaders initReq <> [("Authorization", "Bearer " <> token), ("Content-type","application/json") ],
            requestBody = RequestBodyLBS $ encode  (PostRules { add = Just [rule], delete = Nothing})
          }
      pure r

newtype MyIO a = MyIO {unMyIO :: IO a} deriving (Monad, Functor, MonadIO, Applicative, MonadUnliftIO)

type NaturalTransformation f g = forall a . f a -> g a

filteredStreamPrinted :: IO ()
filteredStreamPrinted = filteredStream printIt unMyIO
  where
  printIt :: (ConduitT StreamEntry StreamEntry MyIO ())
  printIt = iterM (MyIO . print)

filteredStream :: forall m . (MonadIO m) => (ConduitT StreamEntry StreamEntry m ()) -> (NaturalTransformation m IO) -> IO ()
filteredStream doIt nt = do
  manager <- newManager
  let env = Env { manager }
  runAppT env $ do
    req <- liftIO mkReq
    withResponse req handleResponse :: AppT IO ()
  where
    mkReq = do
      initReq <- parseRequest "https://api.twitter.com/2/tweets/search/stream"
      token <- tokenFromEnv
      let r =
            initReq
              { method = "GET",
                requestHeaders = requestHeaders initReq <> [("Authorization", "Bearer " <> token)],
                responseTimeout = responseTimeoutMicro (20 * 1000000) -- one sec
              }
      pure r
    handleResponse :: (MonadIO w) => Response (ConduitT () BS.ByteString m ()) -> AppT w ()
    handleResponse = liftIO . nt . withBody . responseBody
      where
        withBody :: ConduitT _ BS.ByteString m () -> m ()
        withBody body = runConduit ( body .| mapC decodeStrict .| filter isJust .| mapC fromJust .| doIt .| sinkNull )

showEnv :: IO ()
showEnv = do
  print =<< getEnvironment

loadDotEnv :: _ => _ ()
loadDotEnv = void $ loadFile defaultConfig
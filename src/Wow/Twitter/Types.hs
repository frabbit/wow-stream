{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
module Wow.Twitter.Types where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad ( void )
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadPlus (mzero), MonadReader, ReaderT (ReaderT, runReaderT))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Array, Object), decode, encode, object, (.:), (.:?))
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as BS
import Network.HTTP.Client.Conduit
  ( HasHttpManager (getHttpManager),
    Manager,
    RequestBody (RequestBodyLBS),
    Response (responseBody),
    method,
    newManager,
    parseRequest,
    requestBody,
    requestHeaders,
  )
import Network.HTTP.Conduit (httpLbs)
import Polysemy (Member, Sem)
import System.Environment (getEnv, getEnvironment)
import UnliftIO (MonadUnliftIO)
import qualified Wow.Effects.Env as WE
import Prelude hiding (filter)

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

tokenFromEnvPoly :: forall r. (Member WE.Env r) => Sem r BS.ByteString
tokenFromEnvPoly = BS.encodeUtf8 . Text.pack <$> (WE.getEnv "TWITTER_BEARER_TOKEN")

data Rule = Rule
  { ruleId :: Text
  }
  deriving (Show)

instance FromJSON Rule where
  parseJSON (Object o) =
    Rule <$> o .: "id"
  parseJSON _ = mzero

data Rules = Rules
  { rules :: [Rule]
  }
  deriving (Show)

instance FromJSON Rules where
  parseJSON o@(Array _) = do
    rules <- parseJSON o
    pure $ Rules {rules}
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
      let r =
            initReq
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
      let r =
            initReq
              { method = "POST",
                requestHeaders = requestHeaders initReq <> [("Authorization", "Bearer " <> token), ("Content-type", "application/json")],
                requestBody = RequestBodyLBS $ encode PostRules {delete = Just ids, add = Nothing}
              }
      pure r

data RuleDef = RuleDef String deriving (Show)

instance ToJSON RuleDef where
  toJSON (RuleDef s) = object [("value", toJSON s)]

data PostRules = PostRules
  { add :: Maybe [RuleDef],
    delete :: Maybe [RuleId]
  }
  deriving (Show)

data MatchingRule = MatchingRule
  { matchingRuleId :: RuleId,
    tag :: Text
  }
  deriving (Show)

instance FromJSON MatchingRule where
  parseJSON (Object o) = do
    MatchingRule <$> o .: "id" <*> o .: "tag"
  parseJSON _ = mzero

data Tweet = Tweet
  { tweetId :: Text,
    text :: Text
  }
  deriving (Show)

instance FromJSON Tweet where
  parseJSON (Object o) = do
    Tweet <$> o .: "id" <*> o .: "text"
  parseJSON _ = mzero

data StreamEntry = StreamEntry
  { tweet :: Tweet,
    matchingRules :: Maybe [MatchingRule]
  }
  deriving (Show)

instance FromJSON StreamEntry where
  parseJSON (Object o) = do
    tweet <- o .: "data"
    matchingRules <- o .:? "matching_rules"
    pure $ StreamEntry {..}
  parseJSON _ = mzero

instance ToJSON PostRules where
  toJSON pr = do
    let adds = case pr.add of
          Just r -> [("add", toJSON r)]
          Nothing -> []
        deletes = case pr.delete of
          Just ids -> [("delete", object [("ids", toJSON ids)])]
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
      let r =
            initReq
              { method = "POST",
                requestHeaders = requestHeaders initReq <> [("Authorization", "Bearer " <> token), ("Content-type", "application/json")],
                requestBody = RequestBodyLBS $ encode (PostRules {add = Just [rule], delete = Nothing})
              }
      pure r

type NaturalTransformation f g = forall a. f a -> g a

showEnv :: IO ()
showEnv = do
  print =<< getEnvironment

loadDotEnv :: (MonadIO m) => m ()
loadDotEnv = void $ loadFile defaultConfig
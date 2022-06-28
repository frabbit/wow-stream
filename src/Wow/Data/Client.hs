module Wow.Data.Client where

import Wow.Prelude
import Wow.Data.ClientId (ClientId)

data Client = Client {
  name :: Text,
  clientId:: ClientId,
  listening :: Bool,
  tweetFilter :: Maybe Text
  } deriving (Eq, Ord)


initClient :: Text -> ClientId -> Client
initClient n clientId = Client { name = n, clientId, listening = False, tweetFilter = Nothing }

instance Show Client where
  show c = "Client { name = " <> show c.name <> ", listening = " <> show c.listening <> ", tweetFilter = " <> show c.tweetFilter <> "  }"

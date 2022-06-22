module Wow.Data.Client where

import Wow.Prelude
import Wow.Data.ClientId (ClientId)

data Client = Client {
  name :: Text,
  clientId:: ClientId,
  listening :: Bool,
  tweetFilter :: Maybe Text
  }

instance Show Client where
  show c = "Client { name = " <> show c.name <> ", listening = " <> show c.listening <> ", tweetFilter = " <> show c.tweetFilter <> "  }"

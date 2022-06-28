{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Wow.Data.ServerState where

import Wow.Prelude
import Wow.Data.Client (Client)


data ServerState = ServerState {
  clients :: [Client]
} deriving (Show, Eq, Ord)

newServerState :: ServerState
newServerState = ServerState {
  clients = []
}
numClients :: ServerState -> Int
numClients = length . (.clients)

clientExists :: Client -> ServerState -> Bool
clientExists c = any ((== c.name) . (.name)) . (.clients)

nameExists :: Text -> ServerState -> Bool
nameExists name = any ((== name) . (.name)) . (.clients)

addClient :: Client -> ServerState -> ServerState
addClient c s = s{clients = c:s.clients }

removeClient :: Client -> ServerState -> ServerState
removeClient client s = s{ clients = filter ((/= client.name) . (.name)) s.clients }

updateClient :: (Client -> Client) -> ServerState -> ServerState
updateClient update s = s{clients = fmap update s.clients}

updateClientByName :: Client -> (Client -> Client) -> ServerState -> ServerState
updateClientByName c update = updateClient (set c.name)
  where
    set name client
      | client.name /= name = client
      | otherwise = update client

setClientListening :: Client -> Bool -> ServerState -> ServerState
setClientListening c listening = updateClientByName c $ \cl -> cl{listening}

setClientFilter :: Client -> Maybe Text -> ServerState -> ServerState
setClientFilter c tweetFilter = updateClientByName c $ \cl -> cl{tweetFilter}

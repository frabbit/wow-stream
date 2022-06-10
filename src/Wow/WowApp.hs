module Wow.WowApp where


import qualified Data.Text as T
import Prelude
import Network.WebSockets as WS
import Wow.Websocket (newServerState, application, broadcastSilentWhen)
import GHC.Conc (newTVarIO, readTVarIO, forkIO)
import Wow.Twitter (filteredStream, StreamEntry, loadDotEnv)
import Conduit (ConduitT)
import Data.Conduit.Combinators (iterM)

filteredStreamBroadcast :: _ -> IO ()
filteredStreamBroadcast var = filteredStream broadcastC
  where
  broadcastC :: (ConduitT StreamEntry StreamEntry IO ())
  broadcastC = iterM $ \s -> do
      let when client = client.listening && toBool (fmap (`T.isInfixOf` s.tweet.text) client.tweetFilter)
      serverState <- readTVarIO var
      broadcastSilentWhen when s.tweet.text serverState
      pure ()
    where
      toBool (Just x) = x
      toBool _ = False


main :: IO ()
main = do
  loadDotEnv
  state <- newTVarIO newServerState
  _ <- forkIO $ filteredStreamBroadcast state
  WS.runServer "127.0.0.1" 8130 $ application state
module Wow.Websocket.Client where

import Prelude

import Control.Concurrent (forkIO)
import Control.Monad (forever, unless)
import Control.Monad.Trans (liftIO)
import Network.Socket (withSocketsDo)

import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)

clientApp :: WS.ClientApp ()
clientApp conn = do
  putStrLn "connected"

  _ <- forkIO $ forever $ do
    msg <- WS.receiveData conn
    liftIO $ T.putStrLn msg

  let loop = do
        line <- T.getLine
        unless (T.null line) $ WS.sendTextData conn line >> loop

  loop
  WS.sendClose conn ("Bye!" :: Text)


main :: IO ()
main = withSocketsDo $ WS.runClient "127.0.0.1" 8131 "/" clientApp
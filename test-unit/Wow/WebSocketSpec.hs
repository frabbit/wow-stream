{-# HLINT ignore "Move brackets to avoid $" #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Redundant bracket" #-}

module Wow.WebSocketSpec where

import Control.Arrow ((>>>))
import Data.List.Extra (snoc)
import Polysemy (Member, Members, Sem, interpretH, run, raiseUnder)
import Polysemy.AtomicState (AtomicState, atomicStateToState)
import Polysemy.Input (Input, input, runInputList)
import Polysemy.Internal.Tactics (liftT)
import Polysemy.State (State, modify, put, runState)
import Veins.Control.Monad.VExceptT (runVExceptT)
import Veins.Data.VEither (VEither, throwVEither)
import Wow.Data.Client (Client, initClient)
import Wow.Data.ClientId (ClientId (..))
import Wow.Data.Command (Command (CmdClients, CmdFilter, CmdListen, CmdTalk, CmdUnlisten))
import Wow.Data.ServerMessage (ServerMessage)
import Wow.Data.ServerState
  ( ServerState (..),
    addClient,
    newServerState,
  )
import Wow.Effects.ClientChannel (ClientChannel (..), ConnectionNotAvailableError (ConnectionNotAvailableError), InvalidCommandError)
import Wow.Effects.ServerApi (ServerApi (..))
import Wow.TestPrelude
import Wow.Websocket (clientLoop)

sampleClient :: Client
sampleClient = initClient "Joe" (ClientId "abc")

interpretClientChannel :: Members '[Input (Maybe (VEither '[ConnectionNotAvailableError, InvalidCommandError] Command)), State [Wow.Data.ServerMessage.ServerMessage]] r => Sem (ClientChannel ': r) a -> Sem r a
interpretClientChannel = interpretH $ \case
  SendMessage _ t -> liftT $ do
    put @[Wow.Data.ServerMessage.ServerMessage] [t]
    pure . pure $ ()
  ReceiveMessage _ -> liftT $ do
    s <- input
    case s of
      Just x -> do
        pure x
      Nothing ->
        error "no result provided"

interpretClientChannelList :: forall r a .
  [VEither '[ConnectionNotAvailableError, InvalidCommandError] Command] ->
  Members '[State [ServerMessage]] r => Sem (ClientChannel ': r) a -> Sem r a
interpretClientChannelList commands x = interpretClientChannel (raiseUnder x) & runInputList commands

interpretServerApi :: Member (State [String]) r => Sem (ServerApi ': r) a -> Sem r a
interpretServerApi = interpretH $ \x -> case x of
  Listen _ -> handle x
  Unlisten _ -> handle x
  Filter _ _ -> handle x
  ListClients _ -> handle x
  Talk _ _ -> handle x
  where
    handle x = liftT $ do
      modify @[String] (`snoc` show x)
      pure . pure $ ()

data Result a = Result
  { serverState :: ServerState,
    messages :: [ServerMessage],
    apiCalls :: [String],
    result :: a
  }

interpretApp ::
  [VEither '[ConnectionNotAvailableError, InvalidCommandError] Command] ->
  Sem
    [ ServerApi,
      ClientChannel,
      AtomicState ServerState,
      State ServerState,
      State [Wow.Data.ServerMessage.ServerMessage],
      State [String]
    ]
    a ->
  Result a
interpretApp commands x = Result {apiCalls, messages, serverState, result = v}
  where
    (apiCalls, (messages, (serverState, v))) = interp' x
    interp' =
      interpretServerApi
        >>> interpretClientChannelList (commands <> [throwVEither ConnectionNotAvailableError])
        >>> atomicStateToState
        >>> runState (addClient sampleClient newServerState)
        >>> runState []
        >>> runState []
        >>> run

spec :: Spec
spec = describe "Websocket" $ do
  describe "clientLoop should" $ do
    let app :: _ => _
        app = runVExceptT $ clientLoop sampleClient
    it "call listen after receiving CmdListen" $ do
      (interpretApp [pure CmdListen] app).apiCalls `shouldBe` [show $ Listen sampleClient] :: IO ()
    it "call unlisten after receiving CmdUnlisten" $ do
      (interpretApp [pure CmdUnlisten] app).apiCalls `shouldBe` [show $ Unlisten sampleClient] :: IO ()
    it "call filter after receiving CmdFilter" $ do
      (interpretApp [pure $ CmdFilter "abc"] app).apiCalls `shouldBe` [show $ Filter "abc" sampleClient] :: IO ()
    it "call talk after receiving CmdTalk" $ do
      (interpretApp [pure $ CmdTalk "abc"] app).apiCalls `shouldBe` [show $ Talk "abc" sampleClient] :: IO ()
    it "call ListClients after receiving CmdClients" $ do
      (interpretApp [pure CmdClients] app).apiCalls `shouldBe` [show $ ListClients sampleClient] :: IO ()

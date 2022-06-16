{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Wow.Effects.TwitterStream where

import Prelude
import Polysemy (makeSem, Sem, Member, interpretH, runT, getInitialStateT, bindT, pureT, interpret, bindTSimple, raiseUnder, reinterpret2H, raise, raise_, Members, reinterpretH, subsume, Embed, embed)
import qualified Polysemy as PS
import Wow.Twitter.Types (StreamEntry, tokenFromEnvPoly)
import Wow.Effects.HttpLongPolling (HttpLongPolling, Request (Request))
import Wow.Effects.Env (Env)
import qualified Wow.Effects.HttpLongPolling as HLP
import Data.Aeson (decodeStrict)
import Control.Monad (void)
import Polysemy.Internal.Tactics (liftT)
import Control.Lens (Field14(_14))
import Control.Concurrent.STM (readTChan, TChan)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)

data TwitterStream m a where
  TSSampleStream :: (StreamEntry -> m ()) -> TwitterStream m ()

makeSem ''TwitterStream

interpretTwitterStream :: forall r a1 .
  Sem (TwitterStream ': r) a1 -> Sem (Env ': HttpLongPolling ': r) a1
interpretTwitterStream = reinterpret2H $ \case
  TSSampleStream cb -> do
    cb' <- bindT cb
    is <- getInitialStateT
    let
      f1 :: forall a0 r1 . Sem (TwitterStream ': Env ': HttpLongPolling ': r1) a0 -> Sem (Env ': HttpLongPolling ': r1) a0
      f1 x = subsume $ subsume $ interpretTwitterStream (x)
      cb'' :: forall . StreamEntry -> Sem (Env ': HttpLongPolling ': r) ()
      cb'' = \x -> void $ f1 (cb' (x <$ is))
    liftT $ filteredStream cb''

interpretTwitterStreamByTChan :: forall r a1 . (Member (Embed IO) r) =>
  TChan StreamEntry -> Sem (TwitterStream ': r) a1 -> Sem r a1
interpretTwitterStreamByTChan channel = interpretH $ \case
  TSSampleStream cb -> do
    cb' <- bindT cb
    is <- getInitialStateT
    let
      f1 :: forall a0 r0 . (Member (Embed IO) r0) => Sem (TwitterStream ': r0) a0 -> Sem r0 a0
      f1 = interpretTwitterStreamByTChan channel
      cb'' :: forall . StreamEntry -> Sem r ()
      cb'' = \x -> void $ f1 (cb' (x <$ is))
      loop = do
        val <- liftIO . atomically . readTChan $ channel
        cb'' val
        loop
    liftT loop




filteredStream :: forall r . ( Member Env r, Member HttpLongPolling r) =>(StreamEntry -> Sem r ()) -> Sem r ()
filteredStream handler' = do
  let
    handler = handler1 . decodeStrict
      where
        handler1 = \case
          Just x -> handler' x
          Nothing -> pure ()
  token <- tokenFromEnvPoly
  let request = Request {
    HLP.url = "https://api.twitter.com/2/tweets/sample/stream",
    HLP.headers = [("Authorization", "Bearer " <> token)],
    HLP.method = "GET"
  }
  HLP.httpLongPolling request handler
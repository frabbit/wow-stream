module Wow.Effects.Errors where

import Prelude

import Polysemy.Error ( catch, throw, Error, runError )
import Polysemy (Sem, Member, raise)
import Data.Function ((&))
import Polysemy.Internal.Tactics (liftT)

data Err1 = Err1

data Err2 = Err2

data Err3 = Err3

app1 :: (Member (Error Err1) r) => Sem r ()
app1 = throw Err1

app2 :: (Member (Error Err2) r) => Sem r ()
app2 = throw Err2

app3 :: (Member (Error Err3) r) => Sem r ()
app3 = throw Err3

appX :: _ => _
appX = do
  app1
  app2
  app3

recover :: Sem (Error e ': r) a -> (e -> Sem r a) -> Sem r a
recover s f = do
  e <- runError s
  case e of
    Left l -> f l
    Right r -> pure r

recoverAll :: forall r a . Sem (Error Err1 ': Error Err2 ': r) a -> Sem r a -> Sem r a
recoverAll s r1 =
  s `recover` (\(_::Err1) -> raise r1) `recover` (\(_::Err2) -> r1)



appY :: _ => Sem r ()
appY = appX `recover` (\(_::Err3) -> pure ())
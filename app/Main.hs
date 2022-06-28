module Main where

import Wow.Prelude
import qualified Wow.WowApp as WowApp
import Wow.Options (parseCommand)

main :: IO ()
main = do
  cmd <- parseCommand
  let cfg = WowApp.defaultAppConfig{port = cmd.port}
  WowApp.main cfg
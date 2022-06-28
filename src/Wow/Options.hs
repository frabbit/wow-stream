{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Wow.Options where

import Prelude
import Options.Applicative
    ( command,
      defaultPrefs,
      help,
      idm,
      info,
      long,
      metavar,
      progDesc,
      short,
      subparser,
      value,
      execParser,
      execParserPure,
      Parser,
      ParserInfo,
      ParserResult, option, auto )
import GHC.Natural (Natural)

data Command = CmdStart { port :: Natural } deriving (Show, Eq, Ord)

defaultPort :: Natural
defaultPort = 2020


parserStart :: Parser Command
parserStart = CmdStart <$> port
  where
    port = option auto (long "port" <> short 'p' <> metavar "PORT" <> value defaultPort <> help "write to File")
    --arg = argument str (metavar "TARGET...")

parserOpts :: Parser Command
parserOpts = subparser (
  --command "basic" (info parserBasic (progDesc "A basic command") ) <>
  command "start" (info parserStart (progDesc "start the server") )
  )

mainParser :: ParserInfo Command
mainParser = info parserOpts idm

parseCommand :: IO Command
parseCommand = execParser mainParser

parseCommandPure :: [String] -> ParserResult Command
parseCommandPure = execParserPure defaultPrefs mainParser

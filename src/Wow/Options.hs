module Wow.Options where

import Prelude
import Options.Applicative
import Data.Text (Text)

data Command
  = CmdBasic
  | CmdComplex String Text deriving (Show)

parserBasic :: Parser Command
parserBasic = pure CmdBasic

parserComplex :: Parser Command
parserComplex = CmdComplex <$> output <*> arg
  where
    output = strOption (long "output" <> short 'o' <> metavar "FILE" <> value "defaultVal" <> help "write to File")
    arg = argument str (metavar "TARGET...")

parserOpts :: Parser Command
parserOpts = subparser (
  command "basic" (info parserBasic (progDesc "A basic command") ) <>
  command "complex" (info parserComplex (progDesc "A complex command") )
  )

mainParser :: ParserInfo Command
mainParser = info parserOpts idm

parseCommand :: IO Command
parseCommand = execParser mainParser

parseCommandPure :: [String] -> ParserResult Command
parseCommandPure = execParserPure defaultPrefs mainParser

handleCommand :: Command -> IO ()
handleCommand _ = pure ()

main :: IO ()
main = do
  cmd <- parseCommand
  handleCommand cmd


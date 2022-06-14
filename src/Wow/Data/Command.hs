module Wow.Data.Command where

import Data.Text (Text)
import Text.Parsec (ParseError, parse, choice, string, char, eof, many1, oneOf, try)
import Prelude
import Text.Parsec.Text (GenParser)
import Control.Monad (void)
import qualified Data.Text as T

data Command
  = CmdGreeting Text
  | CmdListen
  | CmdFilter Text
  | CmdUnlisten
  | CmdTalk Text
  deriving (Show, Eq, Ord)

eol :: GenParser st ()
eol = void $ char '\n'

commandParser :: GenParser st Command
commandParser = do
  cmd <- choice [listenParser, greetingParser, unlistenParser, filterParser]
  eof
  pure cmd

greetingParser :: GenParser st Command
greetingParser = do
  try . string $ ":greeting"
  char ' '
  name <- many1 (oneOf $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "_-$!.:#")
  pure $ CmdGreeting $ T.pack name

filterParser :: GenParser st Command
filterParser = do
  try . string $ ":filter"
  char ' '
  name <- many1 (oneOf $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "_-$!.:#")
  pure $ CmdFilter $ T.pack name

listenParser :: GenParser st Command
listenParser = do
  try . string $ ":listen"
  pure CmdListen

unlistenParser :: GenParser st Command
unlistenParser = do
  try . string $ ":unlisten"
  pure CmdUnlisten

parseCommand :: Text -> Either ParseError Command
parseCommand = parse commandParser "(unknown)"

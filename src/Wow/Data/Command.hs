{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use <$>" #-}
module Wow.Data.Command where
import Data.Text (Text)
import Text.Megaparsec (Parsec, parse, choice, chunk, eof, oneOf, try, (<?>), some, ParseErrorBundle, ShowErrorComponent (showErrorComponent), (<|>), customFailure, noneOf, option, many)
import Text.Megaparsec.Char (char)
import Prelude
import Control.Monad (void)
import qualified Data.Text as T
import Data.Void (Void)

data Custom = InvalidUsername (Maybe String)
  deriving (Eq, Show, Ord)


instance ShowErrorComponent Custom where
  showErrorComponent (InvalidUsername _) = " Invalid Username"

data Command
  = CmdGreeting Text
  | CmdListen
  | CmdFilter Text
  | CmdUnlisten
  | CmdClients
  | CmdTalk Text
  deriving (Show, Eq, Ord)

type Parser = Parsec Custom Text

toText :: Command -> Text
toText = \case
  CmdGreeting txt -> ":greeting " <> txt
  CmdListen -> ":listen"
  CmdFilter txt -> ":filter " <> txt
  CmdUnlisten -> ":unlisten"
  CmdClients -> ":clients"
  CmdTalk txt -> ":talk " <> txt


eol :: Parser ()
eol = void $ char '\n'

commandParser :: Parser Command
commandParser = do
  cmd <- choice [listenParser, greetingParser, unlistenParser, filterParser, clientsParser, talkParser]
  eof
  pure cmd

greetingParser :: Parser Command
greetingParser = do
  try . chunk $ ":greeting"
  char ' '
  name <- some (oneOf $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "_-$!.:#") <|> do
    badUsername <- option Nothing $ fmap Just $ some (noneOf [' '])
    customFailure $ InvalidUsername badUsername
  pure $ CmdGreeting $ T.pack name

filterParser :: Parser Command
filterParser = do
  try . chunk $ ":filter"
  char ' '
  name <- some (oneOf $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "_-$!.:#")
  pure $ CmdFilter $ T.pack name

clientsParser :: Parser Command
clientsParser = do
  try . chunk $ ":clients"
  pure CmdClients

listenParser :: Parser Command
listenParser = do
  try . chunk $ ":listen"
  pure CmdListen

talkParser :: Parser Command
talkParser = do
  try . chunk $ ":talk"
  char ' '
  msg <- many (noneOf ['\n'])
  pure $ CmdTalk $ T.pack msg

unlistenParser :: Parser Command
unlistenParser = do
  try . chunk $ ":unlisten"
  pure CmdUnlisten

parseCommand :: Text -> Either (ParseErrorBundle Text Custom) Command
parseCommand = parse commandParser "(unknown)"

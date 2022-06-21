{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Wow.Data.ServerMessage where

import Prelude
import Data.Text (Text)
import Text.Megaparsec (ParseErrorBundle, Parsec, many, noneOf, parse, chunk, some, oneOf, choice, try, MonadParsec (eof))
import Data.Void (Void)
import qualified Data.Text as Text
import Text.Megaparsec.Char (char)
import Test.QuickCheck (Arbitrary, oneof, elements)
import Test.QuickCheck.Arbitrary (arbitrary)
import Data.Data (Typeable, Data)

type Custom = Void

data Error
  = ErrUsernameExists
  | ErrInvalidCommand
  deriving (Show,Eq,Typeable, Data)

errorToText :: Error -> Text
errorToText = \case
  ErrUsernameExists -> "UsernameExists"
  ErrInvalidCommand -> "InvalidCommand"

instance Arbitrary Error where
  arbitrary = do
    elements [
      ErrUsernameExists,
      ErrInvalidCommand
      ]

data ServerMessage
  = SMAcknowledge Text
  | SMError Error
  | SMUnexpectedCommand Text
  | SMClientDisconnected Text
  | SMSimpleText Text
  deriving (Show, Eq, Typeable, Data)

instance Arbitrary ServerMessage where
  arbitrary = do
    oneof [
      SMAcknowledge <$> elements ["A", "B"],
      SMError <$> arbitrary,
      SMUnexpectedCommand <$> elements ["A", "B"],
      SMClientDisconnected <$> elements ["A", "B"],
      SMSimpleText <$> elements ["a", "b"]
      ]


type Parser = Parsec Custom Text

notImplemented :: a
notImplemented = error "not implemented"

toText :: ServerMessage -> Text
toText = \case
  SMAcknowledge n -> ":acknowledge " <> n
  SMError e -> ":error " <> errorToText e
  SMUnexpectedCommand cmd -> ":unexpectedCommand " <> cmd
  SMClientDisconnected client -> ":clientDisconnected " <> client
  SMSimpleText t -> t

validCommandIdentifier = some (oneOf $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "_-")

acknowledgeParser :: Parser ServerMessage
acknowledgeParser = do
  try . chunk $ ":acknowledge"
  char ' '
  name <- validCommandIdentifier
  pure $ SMAcknowledge $ Text.pack name

simpleTextParser :: Parser ServerMessage
simpleTextParser = do
  msg <- many (noneOf ['\n'])
  pure $ SMSimpleText $ Text.pack msg

errorParser :: Parser Error
errorParser = do
  choice [
    chunk "UsernameExists" >> pure ErrUsernameExists,
    chunk "InvalidCommand" >> pure ErrInvalidCommand
    ]

errorMsgParser :: Parser ServerMessage
errorMsgParser = do
  try . chunk $ ":error"
  char ' '
  err <- errorParser
  pure $ SMError err

unexpectedCommandParser :: Parser ServerMessage
unexpectedCommandParser = do
  try . chunk $ ":unexpectedCommand"
  char ' '
  name <- many (noneOf ['\n'])
  pure $ SMUnexpectedCommand $ Text.pack name

clientDisconnectedParser :: Parser ServerMessage
clientDisconnectedParser = do
  try . chunk $ ":clientDisconnected"
  char ' '
  name <- many (noneOf ['\n'])
  pure $ SMClientDisconnected $ Text.pack name

serverMessageParser :: Parser ServerMessage
serverMessageParser = do
  command <- choice [
    acknowledgeParser,
    errorMsgParser,
    unexpectedCommandParser,
    clientDisconnectedParser,
    simpleTextParser
    ]
  eof
  pure command

parseServerMessage :: Text -> Either (ParseErrorBundle Text Custom) ServerMessage
parseServerMessage = parse serverMessageParser "(unknown)"
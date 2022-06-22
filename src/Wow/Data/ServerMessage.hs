{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Wow.Data.ServerMessage where

import Prelude
import Data.Text (Text)
import Text.Megaparsec (ParseErrorBundle, Parsec, many, noneOf, parse, chunk, some, oneOf, choice, try, MonadParsec (eof), optional)
import Data.Void (Void)
import qualified Data.Text as Text
import Text.Megaparsec.Char (char)
import Test.QuickCheck (Arbitrary, oneof, elements, choose, vectorOf)
import Test.QuickCheck.Arbitrary (arbitrary)
import Data.Data (Typeable, Data)
import qualified Data.Text as T

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
  | SMClientJoined Text
  | SMSimpleText Text
  | SMClients [Text]
  deriving (Show, Eq, Typeable, Data)

instance Arbitrary ServerMessage where
  arbitrary = do
    oneof [
      SMAcknowledge <$> elements ["A", "B"],
      SMError <$> arbitrary,
      SMUnexpectedCommand <$> elements ["A", "B"],
      SMClientDisconnected <$> elements ["A", "B"],
      SMSimpleText <$> elements ["a", "b"],
      SMClientJoined <$> elements ["A","B"],
      SMClients <$> do
        k <- choose (0,4)
        vectorOf k $ elements ["Pim", "Wim", "Jim", "Tim", "Jill", "Sarah"]
      ]


type Parser = Parsec Custom Text

notImplemented :: a
notImplemented = error "not implemented"

toText :: ServerMessage -> Text
toText = \case
  SMAcknowledge n -> ":acknowledge " <> n
  SMError e -> ":error " <> errorToText e
  SMClients cl -> ":clients " <> T.intercalate "," cl
  SMUnexpectedCommand cmd -> ":unexpectedCommand " <> cmd
  SMClientJoined client -> ":clientJoined " <> client
  SMClientDisconnected client -> ":clientDisconnected " <> client
  SMSimpleText t -> t

validCommandIdentifier :: Parser [Char]
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

clientJoinedParser :: Parser ServerMessage
clientJoinedParser = do
  try . chunk $ ":clientJoined"
  char ' '
  name <- many (noneOf ['\n'])
  pure $ SMClientJoined $ Text.pack name

nameListParser :: Parser [String]
nameListParser = go []
  where
  go :: [String] -> Parser [String]
  go v = do
    name <- optional $ some (noneOf ['\n', ','])
    case name of
      Nothing -> pure $ reverse v
      Just n -> choice [
        char ',' >> go (n : v),
        pure $ reverse (n : v)
        ]

clientsParser :: Parser ServerMessage
clientsParser = do
  try . chunk $ ":clients"
  char ' '
  names <- nameListParser
  pure $ SMClients (fmap T.pack names)

serverMessageParser :: Parser ServerMessage
serverMessageParser = do
  command <- choice [
    acknowledgeParser,
    errorMsgParser,
    unexpectedCommandParser,
    clientDisconnectedParser,
    clientsParser,
    clientJoinedParser,
    simpleTextParser
    ]
  eof
  pure command

parseServerMessage :: Text -> Either (ParseErrorBundle Text Custom) ServerMessage
parseServerMessage = parse serverMessageParser "(unknown)"
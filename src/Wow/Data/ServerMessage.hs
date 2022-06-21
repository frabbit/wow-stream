module Wow.Data.ServerMessage where

import Prelude
import Data.Text (Text)
import Text.Megaparsec (ParseErrorBundle, Parsec, many, noneOf, parse)
import Data.Void (Void)
import qualified Data.Text as Text

type Custom = Void

data Error
  = ErrUsernameExists
  | ErrInvalidCommand

data ServerMessage
  = SMAcknowledge Text
  | SMError Error
  | SMUnexpectedCommand Text
  | SMClientDisconnected Text
  | SMSimpleText Text

type Parser = Parsec Custom Text

notImplemented :: a
notImplemented = error "not implemented"

toText :: ServerMessage -> Text
toText = \case
  SMAcknowledge _ -> notImplemented
  SMError _ -> notImplemented
  SMUnexpectedCommand _ -> notImplemented
  SMClientDisconnected _ -> notImplemented
  SMSimpleText t -> t

serverMessageParser :: Parser ServerMessage
serverMessageParser = do
  msg <- many (noneOf ['\n'])
  pure $ SMSimpleText $ Text.pack msg

parseServerMessage :: Text -> Either (ParseErrorBundle Text Custom) ServerMessage
parseServerMessage = parse serverMessageParser "(unknown)"
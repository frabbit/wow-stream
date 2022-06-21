module Wow.Data.ClientId where

import Wow.Prelude
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)

newtype ClientId = ClientId Text deriving (Show, Eq, Ord)

newClientId :: IO ClientId
newClientId = ClientId . toText <$> nextRandom

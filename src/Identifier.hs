module Identifier where

import           Data.Aeson (toJSON, ToJSON)
import           Data.Char (isSpace)
import           Data.UUID (UUID)
import qualified Data.UUID as UUID

newtype Identifier = Identifier UUID deriving (Eq, Ord)

instance Read Identifier where
  readsPrec _ s =
    let noSpaces = dropWhile isSpace s
    in case UUID.fromString (take 36 noSpaces) of
      Nothing -> []
      Just u  -> [(Identifier u, drop 36 noSpaces)]

instance Show Identifier where
  show (Identifier uuid) = show uuid

instance ToJSON Identifier where
  toJSON identifier = toJSON $ show identifier

module Identifier where

import           Control.Applicative (pure)
import           Data.Aeson
import           Data.Char (isSpace)
import           Data.Text (unpack)
import           Data.UUID (UUID)
import qualified Data.UUID as UUID

newtype Identifier = Identifier UUID deriving (Eq, Ord)

instance Read Identifier where
  readsPrec _ s =
    let noSpaces = dropWhile isSpace s
    in case UUID.fromString (take 36 noSpaces) of
      Just u  -> [(Identifier u, drop 36 noSpaces)]
      Nothing -> []

instance Show Identifier where
  show (Identifier uuid) = show uuid

instance FromJSON Identifier where
  parseJSON = withText "Identifier" $ \t ->
    case UUID.fromString (unpack t) of
      Just uuid -> pure $ Identifier uuid
      _         -> fail "could not parse UUID"

instance ToJSON Identifier where
  toJSON (Identifier uuid) = toJSON $ UUID.toString uuid

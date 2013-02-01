module Core where

import Data.Aeson (toJSON, ToJSON)
import Data.UUID (UUID)

newtype Identifier = Identifier UUID deriving (Eq, Ord, Show)

instance ToJSON Identifier where
  toJSON identifier = toJSON $ show identifier

data Action = Idle | Attack | Move | Turn Int deriving (Show)

type Vector = (Double, Double)

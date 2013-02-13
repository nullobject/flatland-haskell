{-# LANGUAGE DeriveGeneric #-}

module Action where

import Data.Aeson
import GHC.Generics (Generic)

type Direction = Double

data Action =
    Spawn
  | Idle
  | Attack
  | Move
  | Turn Direction
  deriving (Eq, Generic, Show)

instance FromJSON Action
instance ToJSON Action

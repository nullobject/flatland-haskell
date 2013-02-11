{-# LANGUAGE DeriveGeneric #-}

module Action where

import Data.Aeson
import GHC.Generics (Generic)

data Action =
    Spawn
  | Idle
  | Attack
  | Move (Double, Double)
  | Turn Int
  deriving (Eq, Generic, Show)

instance FromJSON Action
instance ToJSON Action

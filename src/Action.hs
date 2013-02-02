{-# LANGUAGE DeriveGeneric #-}

module Action where

import Data.Aeson
import GHC.Generics (Generic)

data Action =
    Idle
  | Attack
  | Move Double
  | Turn Int
  deriving (Generic, Show)

instance FromJSON Action
instance ToJSON Action

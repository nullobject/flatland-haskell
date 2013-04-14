{-# LANGUAGE DeriveGeneric #-}

module Action where

import Data.Aeson
import Geometry
import GHC.Generics (Generic)

data Action =
    Spawn
  | Idle
  | Attack
  | Forward
  | Reverse
  | Turn Angle
  deriving (Eq, Generic, Show)

instance FromJSON Action
instance ToJSON Action

-- Returns the cost of the action.
cost :: Action -> Int
cost action = case action of
  Idle    ->  10
  Attack  -> -30
  Forward -> -20
  Reverse -> -20
  Turn _  -> -20
  _       ->   0

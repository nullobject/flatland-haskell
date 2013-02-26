{-# LANGUAGE DeriveGeneric #-}

module Action where

import Data.Aeson
import GHC.Generics (Generic)

type Direction = Double

data Action =
    Spawn
  | Idle
  | Attack
  | Forward
  | Reverse
  | Turn Direction
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

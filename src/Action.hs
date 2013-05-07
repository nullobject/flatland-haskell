{-# LANGUAGE DeriveGeneric #-}

module Action where

import Collision
import Data.Aeson
import Geometry
import GHC.Generics (Generic)

data Action =
    Update Body -- ^ Update the entity with the body.
  | Spawn       -- ^ Spawn an entity.
  | Idle        -- ^ Don't do anything.
  | Attack      -- ^ Attack in the current direction.
  | Forward     -- ^ Move forwards in the current direction.
  | Reverse     -- ^ Move backwards in the current direction.
  | Turn Angle  -- ^ Turn to the direction.
  deriving (Eq, Generic, Show)

instance FromJSON Action
instance ToJSON Action

-- Returns the cost of the given action.
cost :: Action -> Int
cost action = case action of
  Idle    ->  10
  Attack  -> -30
  Forward -> -20
  Reverse -> -20
  Turn _  -> -20
  _       ->   0

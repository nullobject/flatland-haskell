{-# LANGUAGE DeriveGeneric #-}

module Bullet where

import Geometry (Position, Velocity)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

-- An bullet is an actor in the world.
data Bullet = Bullet
  { bulletPosition  :: Position
  , bulletVelocity  :: Velocity
  } deriving (Generic, Show)

instance ToJSON Bullet

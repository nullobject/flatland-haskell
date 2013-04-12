module Bullet where

import Geometry
import Data.Aeson

-- An bullet is an actor in the world.
data Bullet = Bullet
  { bulletPosition :: Position
  , bulletVelocity :: Velocity
  } deriving (Show)

instance ToJSON Bullet where
  toJSON bullet = object [ "position" .= bulletPosition bullet
                         , "velocity" .= bulletVelocity bullet ]

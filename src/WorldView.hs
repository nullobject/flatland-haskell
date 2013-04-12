module WorldView
  ( forPlayer
  , WorldView (..)
  ) where

import Data.Aeson
import Geometry
import Entity
import Player
import Visibility
import World

data WorldView = WorldView
  { worldViewAge      :: Int
  , worldViewPlayer   :: Player
  , worldViewEntities :: [Entity]
  } deriving (Show)

instance ToJSON WorldView where
  toJSON worldView = object [ "age"      .= worldViewAge      worldView
                            , "player"   .= worldViewPlayer   worldView
                            , "entities" .= worldViewEntities worldView ]

-- Returns a new world view for the given player.
forPlayer :: Player -> World -> WorldView
forPlayer player world =
  WorldView { worldViewAge      = age
            , worldViewPlayer   = player
            , worldViewEntities = visibleEntities
            }

  where age                 = worldAge                 world
        entities            = worldEntities            world
        collisionRectangles = worldCollisionRectangles world

        visibility = case playerEntity player of
                     Just entity -> calculateVisibility (entityPosition entity) collisionRectangles
                     Nothing     -> []

        visibleEntities = filter (\entity -> entityVisible entity visibility) entities

-- Returns true if the entity is in the visibility manifold.
entityVisible :: Entity -> [Triangle] -> Bool
entityVisible entity visibility = any (intersects position) visibility
  where position = entityPosition entity

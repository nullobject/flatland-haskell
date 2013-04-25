module WorldView
  ( newWorldView
  , WorldView (..)
  ) where

import Data.Aeson
import Geometry
import Entity
import Player
import Visibility
import World

-- Represents a player's current view of the world.
data WorldView = WorldView
  {
    -- The age of the world.
    worldViewAge :: Int

    -- The player state.
  , worldViewPlayer :: Player

    -- The entities currently visible to the player.
  , worldViewEntities :: [Entity]

  } deriving (Show)

instance ToJSON WorldView where
  toJSON worldView = object [ "age"      .= worldViewAge      worldView
                            , "player"   .= worldViewPlayer   worldView
                            , "entities" .= worldViewEntities worldView ]

-- Returns a new world view for the given player and world.
newWorldView :: Player -> World -> WorldView
newWorldView player world = WorldView
  { worldViewAge      = age
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

-- Returns true if the entity lies within the visibility manifold.
entityVisible :: Entity -> [Triangle] -> Bool
entityVisible entity visibility = any (intersects position) visibility
  where position = entityPosition entity

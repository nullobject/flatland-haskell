module WorldView
  ( newWorldView
  , WorldView (..)
  ) where

import Core
import Data.Aeson
import Data.Maybe
import Geometry
import Entity
import Player
import Visibility
import World

-- Represents a player's current view of the world.
data WorldView = WorldView
  {
    -- The age of the world.
    worldViewAge :: Age

    -- The player state.
  , worldViewPlayer :: Player

    -- The entities currently visible to the player.
  , worldViewEntities :: [Entity]
  } deriving (Show)

instance ToJSON WorldView where
  toJSON worldView = object [ "age"      .= worldViewAge      worldView
                            , "player"   .= worldViewPlayer   worldView
                            , "entities" .= worldViewEntities worldView
                            ]

-- Returns a new world view for the given player and world.
newWorldView :: Player -> World -> WorldView
newWorldView player world = WorldView { worldViewAge      = age
                                      , worldViewPlayer   = player
                                      , worldViewEntities = entities
                                      }

  where age = worldAge world
        mesh = playerVisibilityMesh player world
        entities = filter predicate $ worldEntities world
        predicate entity = intersectMesh (entityPosition entity) mesh

-- Returns the visibility mesh for the given player and world.
playerVisibilityMesh :: Player -> World -> Mesh
playerVisibilityMesh player world
  -- If the player has not spawned, then they can't see anything.
  | isNothing entity = []

  -- Otherwise, calculate the visibility mesh for the entity.
  | otherwise = calculateVisibilityMesh position staticGeometry

  where entity = case playerEntityId player of
                 Just id -> entityWithId id world
                 Nothing -> Nothing

        position = entityPosition . fromJust $ entity
        staticGeometry = worldStaticGeometry world

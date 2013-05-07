{-# LANGUAGE TupleSections #-}

module World where

import           Action
import           Bullet
import           Collision
import           Control.Wire hiding (object)
import qualified Control.Wire as Wire
import           Core
import           Data.Aeson
import qualified Data.Key as Key
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import           Entity
import           Geometry
import           Identifier
import           Map
import           Player
import           Prelude hiding ((.), id)

type Size = Int

-- Represents the state of the world.
data World = World
  {
    -- The age of the world.
    worldAge :: Age

    -- The layers in the world map.
  , worldLayers :: [Layer]

    -- A map of the entities in the world.
  , worldEntitiesMap :: EntitiesMap

    -- The bullets flying around.
  , worldBullets :: [Bullet]

    -- The collision geometry.
  , worldCollisionRectangles :: [Rectangle]

    -- The positions a player can spawn from.
  , worldSpawnPoints :: [Position]

    -- The world map tile width.
  , worldTileWidth :: Int

    -- The world map tile height.
  , worldTileHeight :: Int
  } deriving (Show)

instance ToJSON World where
  toJSON world = object [ "age"                 .= worldAge world
                        , "layers"              .= worldLayers world
                        , "entities"            .= worldEntities world
                        , "collisionRectangles" .= worldCollisionRectangles world
                        , "tileWidth"           .= worldTileWidth world
                        , "tileHeight"          .= worldTileHeight world
                        ]

-- A world wire takes a list of messages and produces a world state.
type WorldWire = MyWire [Message] World

-- Returns the entities in the world.
worldEntities :: World -> [Entity]
worldEntities world = Map.elems $ worldEntitiesMap world

-- Returns the entity in the world with the given identifier.
entityWithId :: Identifier -> World -> Maybe Entity
entityWithId identifier world = Map.lookup identifier entitiesMap
  where entitiesMap = worldEntitiesMap world

-- Returns a new world state for the given tiled map.
newWorld :: TiledMap -> World
newWorld tiledMap = World { worldAge                 = 0
                          , worldLayers              = layers
                          , worldEntitiesMap         = Map.empty
                          , worldBullets             = []
                          , worldCollisionRectangles = collisionRectangles
                          , worldSpawnPoints         = spawnPoints
                          , worldTileWidth           = tileWidth
                          , worldTileHeight          = tileHeight
                          }

  where layers = Map.getTileLayers tiledMap
        collisionRectangles = Map.getCollisionRectangles tiledMap
        spawnPoints = map rectangleCentre $ Map.getSpawnRectangles tiledMap
        tileWidth = Map.mapTileWidth tiledMap
        tileHeight = Map.mapTileHeight tiledMap

-- Returns a new world wire given an initial world state.
worldWire :: World -> WorldWire
worldWire world = worldWire' world $ entityRouter entityConstructor
  where
    entityConstructor = entityWire $ worldSpawnPoints world

    -- TODO: Step the entities, run physics, then step entities again.
    worldWire' :: World -> EntityRouter -> WorldWire
    worldWire' world entityRouter = mkGen $ \dt messages -> do
      -- Step the entity router with player messages.
      (mx, entityRouter') <- stepWire entityRouter 0 messages

      let entitiesMap = case mx of
                        Left _            -> Map.empty
                        Right entitiesMap -> entitiesMap

      -- Run the physics simulation.
      let bodies  = map entityBody $ Map.elems entitiesMap
          bodies' = runPhysics bodies dt

      -- Step the entity router with 'Update' messages.
      let messages' = map (\body -> (bodyId body, Update body)) bodies
      (mx, entityRouter'') <- stepWire entityRouter' dt messages'

      let entitiesMap' = case mx of
                         Left _            -> Map.empty
                         Right entitiesMap -> entitiesMap

      let world' = world {worldEntitiesMap = entitiesMap'}

      return (Right world', worldWire' world' entityRouter'')

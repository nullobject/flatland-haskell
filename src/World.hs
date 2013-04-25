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
import qualified Data.Traversable as Traversable
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

    -- The players in the world.
  , worldPlayers :: [Player]

    -- The bullets flying around.
  , worldBullets :: [Bullet]

    -- The collision geometry.
  , worldCollisionRectangles :: [Rectangle]

    -- The locations a player can spawn from.
  , worldSpawnRectangles :: [Rectangle]

    -- The world map tile width.
  , worldTileWidth :: Int

    -- The world map tile height.
  , worldTileHeight :: Int

  } deriving (Show)

instance ToJSON World where
  toJSON world = object [ "age"                 .= worldAge                 world
                        , "layers"              .= worldLayers              world
                        , "players"             .= worldPlayers             world
                        , "collisionRectangles" .= worldCollisionRectangles world
                        , "tileWidth"           .= worldTileWidth           world
                        , "tileHeight"          .= worldTileHeight          world ]

-- A world wire takes a list of messages and produces a new world state.
type WorldWire = MyWire [Message] World

-- A route wire routes messages to players.
type RouteWire = MyWire ([AABB], [Message]) [(Player, Maybe Bullet)]

-- A map from an identifier to a player wire.
type PlayerWireMap = Map Identifier PlayerWire

-- Returns a new world.
newWorld :: TiledMap -> World
newWorld tiledMap = World
  { worldAge                 = 0
  , worldLayers              = layers
  , worldPlayers             = []
  , worldBullets             = []
  , worldCollisionRectangles = collisionRectangles
  , worldSpawnRectangles     = spawnRectangles
  , worldTileWidth           = tileWidth
  , worldTileHeight          = tileHeight }

  where layers = Map.getTileLayers tiledMap
        collisionRectangles = Map.getCollisionRectangles tiledMap
        spawnRectangles = Map.getSpawnRectangles tiledMap
        tileWidth = Map.mapTileWidth tiledMap
        tileHeight = Map.mapTileHeight tiledMap

-- Returns the player with the given identifier.
getPlayer :: Identifier -> World -> Maybe Player
getPlayer identifier world = List.find predicate $ worldPlayers world
  where predicate = \player -> playerId player == identifier

-- Returns the entities in the world.
worldEntities :: World -> [Entity]
worldEntities world = Maybe.catMaybes $ map playerEntity $ worldPlayers world

-- Evolves a list of player wires, routing actions which are addressed to them
-- by matching their identifiers. Actions which are addressed to unknown player
-- wires are created using the constructor.
--
-- TODO: Refactor this function.
routeWire :: (Identifier -> PlayerWire) -> RouteWire
routeWire constructor = route Map.empty
  where
    route :: PlayerWireMap -> RouteWire
    route playerWireMap = mkGen $ \dt (objects, messages) -> do
      -- Create a map from identifiers to actions.
      let actionMap = Map.fromList messages

      -- Ensure the messages can be delivered to player wires.
      let playerWireMap' = foldl spawn playerWireMap $ Map.keys actionMap

      -- Step the player wires, supplying the optional actions.
      res <- Key.mapWithKeyM (\identifier wire -> stepWire wire dt (objects, Map.findWithDefault Action.Idle identifier actionMap)) playerWireMap'

      -- WTF does this do?
      let res' = Traversable.sequence . fmap (\(mx, w) -> fmap (, w) mx) $ res

      return (fmap Map.elems (fmap (fmap fst) res'), route (fmap snd res))

    -- Spawns a new player wire if one with the identifier doesn't already exist.
    spawn :: PlayerWireMap -> Identifier -> PlayerWireMap
    spawn playerWireMap identifier = Map.alter f identifier playerWireMap
      where
        f = Just . maybe wire Wire.id
        wire = constructor identifier

-- Returns a new world wire given an initial world state.
worldWire :: World -> WorldWire
worldWire world = proc messages -> do
  age' <- countFrom age0 -< 1
  playerBullets <- wire -< (objects, messages)

  let players' = map fst playerBullets
  let bullets' = Maybe.catMaybes $ map snd playerBullets

  returnA -< world { worldAge     = age'
                   , worldPlayers = players'
                   , worldBullets = bullets' }

  where age0 = worldAge world
        objects = map getRectangleAABB $ worldCollisionRectangles world
        wire = routeWire $ (playerWire $ worldSpawnRectangles world) . newPlayer

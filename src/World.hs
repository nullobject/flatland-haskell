module World where

import           Bullet
import           Collision (getRectangleAABB)
import           Control.Wire hiding (object)
import           Core
import           Data.Aeson
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import           Entity (Entity)
import           Geometry (Rectangle)
import           Identifier
import           Map (Layer, TiledMap)
import qualified Map
import           Player (Player)
import qualified Player
import           Prelude hiding ((.), id)

type Size = Int

-- A world contains a list of players.
data World = World
  { worldAge                 :: Age
  , worldLayers              :: [Layer]
  , worldPlayers             :: [Player]
  , worldBullets             :: [Bullet]
  , worldCollisionRectangles :: [Rectangle]
  , worldSpawnRectangles     :: [Rectangle]
  , worldTileWidth           :: Int
  , worldTileHeight          :: Int
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

-- Returns a new world.
empty :: TiledMap -> World
empty tiledMap =
  World { worldAge                 = 0
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
  where predicate = \player -> Player.id player == identifier

-- Returns the entities in the world.
worldEntities :: World -> [Entity]
worldEntities world = Maybe.catMaybes $ map Player.entity $ worldPlayers world

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
        wire = Player.routeWire $ (Player.playerWire $ worldSpawnRectangles world) . Player.empty

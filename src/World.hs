{-# LANGUAGE DeriveGeneric #-}

module World where

import           Bullet
import           Collision (getRectangleAABB)
import           Control.Wire
import           Core
import           Data.Aeson (ToJSON)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import           Entity (Entity)
import           Geometry (Rectangle)
import           GHC.Generics (Generic)
import           Identifier
import           Map (Layer, TiledMap)
import qualified Map
import           Player (Player)
import qualified Player
import           Prelude hiding ((.), id)

type Size = Int

-- A world contains a list of players.
data World = World
  { age                 :: Age
  , layers              :: [Layer]
  , players             :: [Player]
  , bullets             :: [Bullet]
  , collisionRectangles :: [Rectangle]
  , spawnRectangles     :: [Rectangle]
  , tileWidth           :: Int
  , tileHeight          :: Int
  } deriving (Generic, Show)

instance ToJSON World

-- A world wire takes a list of messages and produces a new world state.
type WorldWire = MyWire [Message] World

-- Returns a new world.
empty :: TiledMap -> World
empty tiledMap =
  World { age                 = 0
        , layers              = layers
        , players             = []
        , bullets             = []
        , collisionRectangles = collisionRectangles
        , spawnRectangles     = spawnRectangles
        , tileWidth           = tileWidth
        , tileHeight          = tileHeight }

  where layers = Map.getTileLayers tiledMap
        collisionRectangles = Map.getCollisionRectangles tiledMap
        spawnRectangles = Map.getSpawnRectangles tiledMap
        tileWidth = Map.mapTileWidth tiledMap
        tileHeight = Map.mapTileHeight tiledMap

-- Returns the player with the given identifier.
getPlayer :: Identifier -> World -> Maybe Player
getPlayer identifier world = List.find predicate $ players world
  where predicate = \player -> Player.id player == identifier

-- Returns the entities in the world.
entities :: World -> [Entity]
entities world = Maybe.catMaybes $ map Player.entity $ players world

-- Returns a new world wire given an initial world state.
worldWire :: World -> WorldWire
worldWire world = proc messages -> do
  age' <- countFrom age0 -< 1
  playerBullets <- wire -< (objects, messages)

  let players' = map fst playerBullets
  let bullets' = Maybe.catMaybes $ map snd playerBullets

  returnA -< world { age     = age'
                   , players = players'
                   , bullets = bullets' }

  where age0 = age world
        objects = map getRectangleAABB $ collisionRectangles world
        wire = Player.routeWire $ (Player.playerWire $ spawnRectangles world) . Player.empty

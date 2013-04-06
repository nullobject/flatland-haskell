{-# LANGUAGE DeriveGeneric #-}

module World where

import           Collision (calculateAABB)
import           Control.Wire
import           Core
import           Data.Aeson (ToJSON)
import           Data.Bits
import qualified Data.Key as Key
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import           Data.VectorSpace
import           Entity (Entity)
import           Geometry (Polygon)
import           GHC.Generics (Generic)
import           Identifier
import           Player (Player)
import qualified Player
import           Prelude hiding ((.), id)
import qualified Tiled

type Size = Int

data Tile = Tile
  { position :: (Int, Int)
  , gid      :: Int } deriving (Generic, Show)

instance ToJSON Tile

data Layer = Layer
  { name :: String
  , tiles :: [Tile] } deriving (Generic, Show)

instance ToJSON Layer

-- A world contains a list of players.
data World = World
  { age      :: Age
  , players  :: [Player]
  , polygons :: [Polygon]
  , layers   :: [Layer]
  } deriving (Generic, Show)

instance ToJSON World

-- A world wire takes a list of messages and produces a new world state.
type WorldWire = MyWire [Message] World

-- Returns a new world.
empty :: Tiled.TiledMap -> World
empty tileMap =
  World { age      = 0
        , players  = []
        , polygons = polygons
        , layers   = layers }

  where -- gids = map (fromIntegral . Tiled.tileGid) tiles
        -- tiles = Map.elems $ Tiled.layerData $ Tiled.getLayer tileMap "background"
        layers = map toLayer $ Tiled.getTileLayers tileMap
        polygons = map Tiled.calculatePolygon $ Tiled.layerObjects $ Tiled.getLayer tileMap "collision"

toLayer layer = Layer name tiles
  where name = Tiled.layerName layer
        tiles = Key.foldMapWithKey toTile $ Tiled.layerData layer
        toTile position tile = [Tile position ((fromIntegral . Tiled.tileGid) tile)]

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
  players' <- Player.routeWire $ Player.playerWire . Player.empty -< (objects, messages)

  returnA -< world { age     = age'
                   , players = players' }

  where age0    = age world
        objects = map calculateAABB $ polygons world

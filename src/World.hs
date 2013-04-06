{-# LANGUAGE DeriveGeneric #-}

module World where

import           Collision (calculateAABB)
import           Control.Wire
import           Core
import           Data.Aeson (ToJSON)
import           Data.Bits
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

-- A tile represents a cell in the world grid. The bits of the tile value
-- represent properties of the tile:
--
-- bit 7    - Whether a player can spawn in the tile.
-- bit 6    - Whether the tile is opaque to entities.
-- bit 5    - Whether the tile can collide with entities.
-- bits 4-0 - The tile index.
type Tile = Int

type Size = Int

-- A world contains a list of players.
data World = World
  { age      :: Age
  , players  :: [Player]
  , polygons :: [Polygon]
  , tiles    :: [Tile]
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
        , tiles    = gids }

  where gids = map (fromIntegral . Tiled.tileGid) tiles
        tiles = Map.elems $ Tiled.layerData $ Tiled.getLayer tileMap "background"
        polygons = map Tiled.calculatePolygon $ Tiled.layerObjects $ Tiled.getLayer tileMap "collision"

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

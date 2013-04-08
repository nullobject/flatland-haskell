{-# LANGUAGE DeriveGeneric #-}

module Map
  ( getCollisionRectangles
  , getSpawnRectangles
  , getTileLayers
  , Layer
  , Tile
  , T.loadMapFile
  , T.mapTileWidth
  , T.mapTileHeight
  , T.TiledMap
  ) where

import           Data.Aeson (ToJSON)
import qualified Data.Key as Key
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Tiled as T
import           Geometry (Extents, Rectangle (..))
import           GHC.Generics (Generic)

data Tile = Tile
  { position :: (Int, Int)
  , gid      :: Int
  , hFlipped :: Bool
  , vFlipped :: Bool
  , dFlipped :: Bool } deriving (Generic, Show)

instance ToJSON Tile

data Layer = Layer
  { name :: String
  , tiles :: [Tile] } deriving (Generic, Show)

instance ToJSON Layer

-- Returns the collision rectangles for the tiled map.
getCollisionRectangles :: T.TiledMap -> [Rectangle]
getCollisionRectangles tiledMap = map (calculateRectangle extents) $ T.layerObjects $ getLayer "collision" tiledMap
  where extents = getTileExtents tiledMap

-- Returns the spawn rectangles for the tiled map.
getSpawnRectangles :: T.TiledMap -> [Rectangle]
getSpawnRectangles tiledMap = map (calculateRectangle extents) $ T.layerObjects $ getLayer "spawn" tiledMap
  where extents = getTileExtents tiledMap

getTileExtents :: T.TiledMap -> Extents
getTileExtents tiledMap = (width, height)
  where width  = fromIntegral $ T.mapTileWidth  tiledMap
        height = fromIntegral $ T.mapTileHeight tiledMap

-- Calculates a bounding rectangle for the given tiled object.
calculateRectangle :: Extents -> T.Object -> Rectangle
calculateRectangle (tileWidth, tileHeight) object = Rectangle (x, y) (width, height)
  where x = (fromIntegral $ T.objectX object) / tileWidth
        y = (fromIntegral $ T.objectY object) / tileHeight

        width  = (fromIntegral $ Maybe.fromJust $ T.objectWidth  object) / tileWidth
        height = (fromIntegral $ Maybe.fromJust $ T.objectHeight object) / tileHeight

-- Returns the tile layers for the given tiled map.
getTileLayers :: T.TiledMap -> [Layer]
getTileLayers tiledMap = map toLayer $ filter isTileLayer layers
  where layers = T.mapLayers tiledMap

toLayer layer = Layer name tiles
  where name = T.layerName layer
        tiles = Key.foldMapWithKey (\position tile -> [toTile position tile]) $ T.layerData layer

toTile position tile = Tile position gid hFlipped vFlipped dFlipped
  where gid = fromIntegral . T.tileGid $ tile
        hFlipped = T.tileIsHFlipped tile
        vFlipped = T.tileIsVFlipped tile
        dFlipped = T.tileIsDiagFlipped tile

getLayer :: String -> T.TiledMap -> T.Layer
getLayer name tiledMap = Maybe.fromJust $ List.find predicate layers
  where predicate layer = T.layerName layer == name
        layers = T.mapLayers tiledMap

isTileLayer :: T.Layer -> Bool
isTileLayer (T.Layer _ _ _ _ _) = True
isTileLayer _                   = False

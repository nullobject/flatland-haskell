{-# LANGUAGE DeriveGeneric #-}

module Map
  ( getCollisionPolygons
  , getTileLayers
  , Layer
  , Tile
  , T.TiledMap
  ) where

import           Data.Aeson (ToJSON)
import qualified Data.Key as Key
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Tiled.Types as T
import           Geometry (Extents, Polygon (..))
import           GHC.Generics (Generic)

data Tile = Tile
  { position :: (Int, Int)
  , gid      :: Int } deriving (Generic, Show)

instance ToJSON Tile

data Layer = Layer
  { name :: String
  , tiles :: [Tile] } deriving (Generic, Show)

instance ToJSON Layer

-- Returns the collision polygons for the given tiled map.
getCollisionPolygons :: T.TiledMap -> [Polygon]
getCollisionPolygons tiledMap = map (calculatePolygon extents) $ T.layerObjects $ getLayer "collision" tiledMap
  where extents = getTileExtents tiledMap

getTileExtents :: T.TiledMap -> Extents
getTileExtents tiledMap = (width, height)
  where width  = fromIntegral $ T.mapTileWidth  tiledMap
        height = fromIntegral $ T.mapTileHeight tiledMap

-- Calculates a bounding polygon for the given tiled object.
calculatePolygon :: Extents -> T.Object -> Polygon
calculatePolygon (tileWidth, tileHeight) object =
  Polygon [ (x,         y)
          , (x + width, y)
          , (x,         y + height)
          , (x + width, y + height) ]

  where x = (fromIntegral $ T.objectX object) / tileWidth
        y = (fromIntegral $ T.objectY object) / tileHeight

        width  = (fromIntegral $ Maybe.fromJust $ T.objectWidth  object) / tileWidth
        height = (fromIntegral $ Maybe.fromJust $ T.objectHeight object) / tileHeight

-- Returns the tile layers for the given tiled map.
getTileLayers :: T.TiledMap -> [Layer]
getTileLayers tiledMap = map Map.toLayer $ filter isTileLayer layers
  where layers = T.mapLayers tiledMap

getLayer :: String -> T.TiledMap -> T.Layer
getLayer name tiledMap = Maybe.fromJust $ List.find predicate layers
  where predicate layer = T.layerName layer == name
        layers = T.mapLayers tiledMap

toLayer layer = Layer name tiles
  where name = T.layerName layer
        tiles = Key.foldMapWithKey toTile $ T.layerData layer
        toTile position tile = [Tile position ((fromIntegral . T.tileGid) tile)]

isTileLayer :: T.Layer -> Bool
isTileLayer (T.Layer _ _ _ _ _) = True
isTileLayer _                 = False

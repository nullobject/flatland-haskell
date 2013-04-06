module Tiled
  ( getLayer
  , calculatePolygon
  , module T
  ) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import           Data.Tiled as T
import qualified Geometry (Polygon (..))

-- Returns the layer in the tiled map with the given name.
getLayer :: T.TiledMap -> String -> T.Layer
getLayer tileMap name = Maybe.fromJust $ List.find predicate layers
  where predicate layer = T.layerName layer == name
        layers = T.mapLayers tileMap

-- Calculates a bounding polygon for the given object.
calculatePolygon :: T.Object -> Geometry.Polygon
calculatePolygon object = Geometry.Polygon [ (x,         y)
                                           , (x + width, y)
                                           , (x,         y + height)
                                           , (x + width, y + height) ]

  where x = (fromIntegral $ T.objectX object) / 16
        y = (fromIntegral $ T.objectY object) / 16

        width  = (fromIntegral $ Maybe.fromJust $ T.objectWidth  object) / 16
        height = (fromIntegral $ Maybe.fromJust $ T.objectHeight object) / 16

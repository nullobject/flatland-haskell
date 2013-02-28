{-# LANGUAGE DeriveGeneric #-}

module WorldView
  ( forPlayer
  , WorldView (..)
  ) where

import           Data.Aeson (ToJSON)
import           Geometry (Segment (..), Triangle (..))
import qualified Geometry
import           GHC.Generics (Generic)
import           Entity (Entity)
import qualified Entity
import           Player (Player)
import qualified Player
import qualified Visibility
import           World (World)
import qualified World

data WorldView = WorldView
  { age      :: Int
  , player   :: Player
  , entities :: [Entity]
  , segments :: [Segment]
  } deriving (Generic, Show)

instance ToJSON WorldView

-- Returns a new world view for the given player.
forPlayer :: Player -> World -> WorldView
forPlayer player world =
  WorldView { age      = age
            , player   = player
            , entities = visibleEntities
            , segments = visibleSegments
            }

  where age      = World.age world
        entities = World.entities world
        segments = World.segments world

        visibility = case Player.entity player of
                     Just entity -> Visibility.calculateVisibility (Entity.position entity) segments
                     Nothing     -> []

        visibleEntities = filter (\entity -> entityVisible entity visibility) entities
        visibleSegments = map (\(Triangle a b c) -> Segment b c) visibility

-- Returns true if the entity is in the visibility polygon.
entityVisible :: Entity -> [Triangle] -> Bool
entityVisible entity visibility = any (Geometry.intersects position) visibility
  where position = Entity.position entity

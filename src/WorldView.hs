module WorldView
  ( forPlayer
  , WorldView (..)
  ) where

import           Data.Aeson
import           Geometry (Triangle)
import qualified Geometry
import           Entity (Entity)
import qualified Entity
import           Player (Player)
import qualified Player
import qualified Visibility
import           World (World)
import qualified World

data WorldView = WorldView
  { worldViewAge      :: Int
  , worldViewPlayer   :: Player
  , worldViewEntities :: [Entity]
  } deriving (Show)

instance ToJSON WorldView where
  toJSON worldView = object [ "age"      .= worldViewAge      worldView
                            , "player"   .= worldViewPlayer   worldView
                            , "entities" .= worldViewEntities worldView ]

-- Returns a new world view for the given player.
forPlayer :: Player -> World -> WorldView
forPlayer player world =
  WorldView { worldViewAge      = age
            , worldViewPlayer   = player
            , worldViewEntities = visibleEntities
            }

  where age                 = World.worldAge                 world
        entities            = World.worldEntities            world
        collisionRectangles = World.worldCollisionRectangles world

        visibility = case Player.playerEntity player of
                     Just entity -> Visibility.calculateVisibility (Entity.position entity) collisionRectangles
                     Nothing     -> []

        visibleEntities = filter (\entity -> entityVisible entity visibility) entities

-- Returns true if the entity is in the visibility manifold.
entityVisible :: Entity -> [Triangle] -> Bool
entityVisible entity visibility = any (Geometry.intersects position) visibility
  where position = Entity.position entity

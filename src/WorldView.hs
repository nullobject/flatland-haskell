{-# LANGUAGE DeriveGeneric #-}

module WorldView where

import           Data.Aeson (ToJSON)
import           GHC.Generics (Generic)
import           Entity (Entity)
import           Player (Player)
import           World (World)
import qualified World

data WorldView = WorldView
  { age      :: Int
  , entities :: [Entity]
  , player   :: Player
  } deriving (Generic, Show)

instance ToJSON WorldView

-- Returns a new world view for the given player.
forPlayer :: Player -> World -> WorldView
forPlayer player world = WorldView
  { age      = World.age world
  , entities = World.entities world
  , player   = player
  }

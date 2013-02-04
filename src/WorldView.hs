{-# LANGUAGE DeriveGeneric #-}

module WorldView where

import           Data.Aeson (ToJSON)
import           GHC.Generics (Generic)
import           Entity (Entity)
import           World (World)
import qualified World

data WorldView = WorldView
  { age      :: Int
  , entities :: [Entity]
  } deriving (Generic, Show)

instance ToJSON WorldView

-- Returns a new world view for the given player in the world.
fromWorld :: World -> WorldView
fromWorld world = WorldView
  { age      = World.age world
  , entities = World.entities world
  }

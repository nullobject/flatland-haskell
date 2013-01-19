{-# LANGUAGE DeriveGeneric #-}

module WorldView where

import Data.Aeson (ToJSON)
import Data.Map (elems)
import GHC.Generics (Generic)
import Entity (Entity)
import World (WorldState)
import qualified World

data WorldView = WorldView {
  entities :: [Entity],
  age     :: Int
} deriving (Generic, Show)

instance ToJSON WorldView

-- Returns a new world view for the given world.
fromWorld :: WorldState -> WorldView
fromWorld world = WorldView {
  entities = elems $ World.entities world,
  age      = World.age world
}

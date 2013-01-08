{-# LANGUAGE DeriveGeneric #-}

module WorldView where

import Data.Aeson (ToJSON)
import Data.Map (elems)
import GHC.Generics (Generic)
import Player (Player)
import World (World)
import qualified World

data WorldView = WorldView {
  players :: [Player],
  age     :: Int
} deriving (Generic, Show)

instance ToJSON WorldView

-- Returns a new world view for the given world.
fromWorld :: World -> WorldView
fromWorld world = WorldView {
  players = elems $ World.players world,
  age     = World.age world
}

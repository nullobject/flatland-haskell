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

-- Returns a new world view for the given player in the world.
--
-- TODO: return the given player.
fromWorld :: World -> WorldView
fromWorld world = WorldView
  { age      = World.age world
  , entities = World.entities world
  , player   = head $ World.players world
  }

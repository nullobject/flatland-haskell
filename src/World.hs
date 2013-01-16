module World where

import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import           Player (Player)
import qualified Player

data World = World {
  players :: Map UUID Player,
  age     :: Int
} deriving (Show)

type WorldState = StateT World IO

-- Returns a new world.
empty :: World
empty = World {
  players = Map.empty,
  age     = 0
}

-- Increments the age of the world.
incrementAge :: World -> World
incrementAge world = world {age = age'}
  where age' = age world + 1

-- Ticks the players in the world.
tickPlayers :: World -> World
tickPlayers world = world {players = players'}
  where players' = Map.map Player.tick $ players world

-- Adds a player to the world.
addPlayer :: Player -> World -> World
addPlayer player world = world {players = players'}
  where
    uuid = Player.id player
    players' = Map.insert uuid player $ players world

-- Initialises the world.
initWorld :: WorldState ()
initWorld = do
  uuid <- liftIO $ UUID.nextRandom
  World.spawn $ Player.empty uuid

-- Ticks the world.
tick :: WorldState ()
tick = modify $ incrementAge . tickPlayers

-- Spawns a player in the world.
spawn :: Player -> WorldState ()
spawn player = modify $ addPlayer player

-- Moves a player in the world.
move :: UUID -> WorldState ()
move uuid = do
  world <- get
  let players' = Map.adjust Player.move uuid $ players world
  put world {players = players'}

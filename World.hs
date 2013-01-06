module World where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Player (Player)
import qualified Player

data World = World {
  players :: Map UUID Player,
  age     :: Int
} deriving (Show)

type WorldState = StateT World IO

-- Returns a new world.
empty :: IO World
empty = do
  return World {
    players = Map.empty,
    age     = 0
  }

incrementAge :: World -> World
incrementAge world = world {age = age'}
  where age' = age world + 1

tickPlayers :: World -> World
tickPlayers world = world {players = players'}
  where players' = Map.map Player.tick $ players world

addPlayer :: UUID -> Player -> World -> World
addPlayer uuid player world = world {players = players'}
  where players' = Map.insert uuid player $ players world

-- Ticks the world state.
tick :: WorldState ()
tick = modify $ incrementAge . tickPlayers

-- Spawns the given player.
spawn :: Player -> WorldState ()
spawn player = lift nextRandom >>= \uuid -> modify $ addPlayer uuid player

-- Moves the player with the given ID.
move :: UUID -> WorldState ()
move uuid = do
  world <- get
  let uuid' = head $ Map.keys $ players world
  let players' = Map.adjust Player.move uuid' $ players world
  put world {players = players'}

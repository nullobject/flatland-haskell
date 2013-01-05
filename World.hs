module World where

import Control.Monad.State
import Player (Player, tick)

data World = World {
  players :: [Player],
  age     :: Int
} deriving (Show)

type WorldState = StateT World IO ()

initWorld :: IO World
initWorld = do
  return World {players = [], age = 0}

incrementAge :: World -> World
incrementAge world = world {age = age world + 1}

tickPlayers :: World -> World
tickPlayers world = world {players = map Player.tick $ players world}

addPlayer :: Player -> World -> World
addPlayer player world = world {players = player : players world}

-- Ticks the world state.
tick :: WorldState
tick = modify $ incrementAge . tickPlayers

-- Spawns the given player.
spawn :: Player -> WorldState
spawn player = modify $ addPlayer player

-- Moves the player with the given ID.
move :: String -> WorldState
move id = return ()

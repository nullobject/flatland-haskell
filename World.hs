{-# LANGUAGE FlexibleContexts #-}

module World where

import Control.Monad.State
import Player (Player, tick)

data World = World {
  players :: [Player],
  age     :: Int
} deriving (Show)

-- newtype WorldState a = WorldState (StateT World IO a)

initWorld :: World
initWorld = World {players = [], age = 0}

incrementAge :: World -> World
incrementAge world = world {age = age world + 1}

tickPlayers :: World -> World
tickPlayers world = world {players = map Player.tick $ players world}

addPlayer :: Player -> World -> World
addPlayer player world = world {players = player : players world}

-- Ticks the world state.
tick :: (MonadState World m) => m ()
tick = modify $ incrementAge . tickPlayers

-- Spawns the given player.
spawn :: (MonadState World m) => Player -> m ()
spawn player = modify $ addPlayer player

-- Moves the player with the given ID.
move :: (MonadState World m) => String -> m ()
move id = return ()

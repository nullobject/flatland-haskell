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
incrementAge w = w {age = a + 1}
  where a = age w

tickPlayers :: World -> World
tickPlayers w = w {players = Map.map Player.tick ps}
  where ps = players w

addPlayer :: UUID -> Player -> World -> World
addPlayer uuid p w = w {players = Map.insert uuid p ps}
  where ps = players w

-- Ticks the world state.
tick :: WorldState ()
tick = modify $ incrementAge . tickPlayers

-- Spawns the given player.
spawn :: Player -> WorldState ()
spawn p = lift nextRandom >>= \uuid -> modify $ addPlayer uuid p

-- Moves the player with the given ID.
move :: UUID -> WorldState ()
move uuid = do
  w <- get
  let uuid' = head $ Map.keys $ players w
  let ps = Map.adjust Player.move uuid' $ players w
  put w {players = ps}

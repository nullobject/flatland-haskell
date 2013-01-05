module Player where

import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)

type Age    = Int
type Health = Int
type Vector = (Int, Int)

data StateName = Dead | Idle deriving (Show)

data Player = Player {
  id        :: UUID,
  stateName :: StateName,
  health    :: Health,
  position  :: Vector,
  age       :: Age
} deriving (Show)

initPlayer :: IO Player
initPlayer = do
  uuid <- nextRandom
  return Player {Player.id = uuid, stateName = Idle, health = 100, position = (0, 0), age = 0}

-- Ticks the player state.
tick :: Player -> Player
tick player = player {age = age player + 1}

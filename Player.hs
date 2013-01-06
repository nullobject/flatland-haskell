module Player where

type Age    = Int
type Health = Int
type Vector = (Double, Double)

data StateName = Dead | Idle deriving (Eq, Show)

data Player = Player {
  stateName :: StateName,
  health    :: Health,
  position  :: Vector,
  age       :: Age
} deriving (Show)

-- Returns a new player.
empty :: IO Player
empty = do
  return Player {
    stateName = Idle,
    health    = 100,
    position  = (0, 0),
    age       = 0
  }

-- Increments the age of the player.
incrementAge :: Player -> Player
incrementAge player = player {age = age'}
  where age' = age player + 1

-- Ticks the player.
tick :: Player -> Player
tick player = incrementAge player

-- Moves the player.
move :: Player -> Player
move player = player {position = (x + 1, y + 1)}
  where (x, y) = position player

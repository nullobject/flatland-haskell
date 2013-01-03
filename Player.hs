module Player where

type Health = Int
type Vector = (Int, Int)

data StateName = Dead | Idle deriving (Show)

data Player = Player {
  stateName :: StateName,
  health    :: Health,
  position  :: Vector,
  age       :: Int
} deriving (Show)

initPlayer = Player {stateName = Idle, health = 100, position = (0, 0), age = 0}

tick :: Player -> Player
tick player = player {age = age player + 1}

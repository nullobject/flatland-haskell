{-# LANGUAGE DeriveGeneric #-}

module Player where

import Data.Aeson (toJSON, ToJSON)
import Data.Char (toLower)
import GHC.Generics (Generic)

type Age    = Int
type Health = Int
type Vector = (Int, Int)

data StateName = Dead | Idle deriving (Eq, Generic, Show)

instance ToJSON StateName where
  toJSON s = toJSON $ map toLower $ show s

data Player = Player {
  stateName :: StateName,
  health    :: Health,
  position  :: Vector,
  age       :: Age
} deriving (Generic, Show)

instance ToJSON Player

-- Returns a new player.
empty :: Player
empty = Player {
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

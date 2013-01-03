module World where

import Control.Monad.State
import Player (Player, tick)

data World = World {
  players :: [Player],
  age     :: Int
} deriving (Show)

newtype WorldMonad a = WorldMonad (StateT World IO a)

initWorld = World {players = [], age = 0}

getPlayers :: StateT World IO [Player]
getPlayers = players `liftM` get

setPlayers :: [Player] -> StateT World IO ()
setPlayers players = get >>= \state -> put (state {players = players})

getAge :: StateT World IO Int
getAge = age `liftM` get

setAge :: Int -> StateT World IO ()
setAge age = get >>= \state -> put (state {age = age})

spawn :: Player -> StateT World IO ()
spawn player = getPlayers >>= setPlayers . (player:)

tick :: StateT World IO ()
tick = do
  a <- getAge
  setAge (a + 1)
  p <- getPlayers
  let p' = map Player.tick p
  setPlayers p'

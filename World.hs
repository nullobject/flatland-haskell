{-# LANGUAGE FlexibleContexts #-}

module World where

import Control.Monad.State
import Player (Player, tick)

data World = World {
  players :: [Player],
  age     :: Int
} deriving (Show)

newtype WorldMonad a = WorldMonad (StateT World IO a)

initWorld = World {players = [], age = 0}

getPlayers :: (MonadState World) m => m [Player]
getPlayers = players `liftM` get

setPlayers :: (MonadState World) m => [Player] -> m ()
setPlayers players = get >>= \state -> put (state {players = players})

getAge :: (MonadState World) m => m Int
getAge = age `liftM` get

setAge :: (MonadState World) m => Int -> m ()
setAge age = get >>= \state -> put (state {age = age})

spawn :: (MonadState World) m => Player -> m ()
spawn player = getPlayers >>= setPlayers . (player:)

tick :: (MonadState World) m => m ()
tick = do
  a <- getAge
  setAge (a + 1)
  p <- getPlayers
  let p' = map Player.tick p
  setPlayers p'

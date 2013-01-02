{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State

type Health = Int
type Vector = (Int, Int)

data StateName = Dead | Idle deriving (Show)

data PlayerState = PlayerState {
  stateName :: StateName,
  health    :: Health,
  position  :: Vector
} deriving (Show)

newtype PlayerStateMonad a = PlayerStateMonad (StateT PlayerState IO a)

getHealth :: (MonadState PlayerState) m => m Int
getHealth = health `liftM` get

setHealth :: (MonadState PlayerState) m => Int -> m ()
setHealth health = get >>= \state -> put (state {health = health})

initPlayerState = PlayerState {stateName = Idle, health = 100, position = (0, 0)}

-- runStateT (setHealth 200) initPlayerState

main = do
  putStrLn "hello"

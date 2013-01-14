module Types where

import Control.Concurrent.STM
import Data.UUID (UUID)
import WorldView (WorldView)

data Action = Idle | Attack | Move | Turn Int deriving (Show)

data Message = ActionMessage Action UUID | WorldViewMessage WorldView deriving (Show)

type Reply = TMVar Message

data GameRequest = GameRequest Reply Message

-- Drains the channel and returns the messages.
drainTChan :: TChan a -> STM [a]
drainTChan chan = do
  empty <- isEmptyTChan chan
  if empty then return [] else drain
  where
    drain = do
      x <- readTChan chan
      xs <- drainTChan chan
      return (x:xs)

-- Replies to a message.
tell :: Reply -> Message -> IO ()
tell sender message = atomically $ putTMVar sender message

-- Writes a message to the channel and waits for a response.
ask :: TChan GameRequest -> Message -> IO Message
ask chan message = do
  sender <- newEmptyTMVarIO
  atomically $ writeTChan chan $ GameRequest sender message
  atomically $ takeTMVar sender

drain :: TChan GameRequest -> IO [GameRequest]
drain chan = atomically $ drainTChan chan

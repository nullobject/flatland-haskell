module Core where

import Control.Concurrent.STM
import Data.UUID (UUID)
import WorldView (WorldView)

data Action = Idle | Attack | Move | Turn Int deriving (Show)

data Message = ActionMessage Action UUID | WorldViewMessage WorldView deriving (Show)

data Request = Request Response Message

type Response = TMVar Message

drainTChan :: TChan a -> STM [a]
drainTChan chan = do
  empty <- isEmptyTChan chan
  if empty then return [] else drainTChan'
  where
    drainTChan' = do
      x <- readTChan chan
      xs <- drainTChan chan
      return (x:xs)

-- Drains the channel and returns the messages.
drain :: TChan Request -> IO [Request]
drain chan = atomically $ drainTChan chan

-- Replies to a message.
tell :: Response -> Message -> IO ()
tell sender message = atomically $ putTMVar sender message

-- Writes a message to the channel and waits for a response.
ask :: TChan Request -> Message -> IO Message
ask chan message = do
  sender <- newEmptyTMVarIO
  atomically $ writeTChan chan $ Request sender message
  atomically $ takeTMVar sender

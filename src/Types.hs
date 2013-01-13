module Types where

import Control.Concurrent.STM
import WorldView (WorldView)

type Sender = TMVar Message

data Action = Idle | Attack | Move | Turn Int deriving (Show)

data Request = Request Sender Message

type RequestChan = TChan Request

data Message = ActionMessage Action | WorldViewMessage WorldView deriving (Show)

oneSecond :: Int
oneSecond = 1000000

-- Drains the channel and returns the messages.
drainTChan :: TChan a -> STM [a]
drainTChan chan = do
  empty <- isEmptyTChan chan
  if empty
  then return []
  else do
    x <- readTChan chan
    xs <- drainTChan chan
    return (x:xs)

-- Sends a message to a sender.
tell :: Sender -> Message -> STM ()
tell sender message = putTMVar sender message

-- Writes a message to the channel and waits for a response.
ask :: RequestChan -> Message -> IO Message
ask chan message = do
  sender <- newEmptyTMVarIO
  atomically $ writeTChan chan $ Request sender message
  atomically $ takeTMVar sender

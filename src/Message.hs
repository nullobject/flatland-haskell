module Message where

import Control.Concurrent.STM
import Core
import WorldView

data Request = Request Message Response

type Response = TMVar Message

data Message = ActionMessage Action Identifier | WorldViewMessage WorldView deriving (Show)

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
  atomically $ writeTChan chan $ Request message sender
  atomically $ takeTMVar sender

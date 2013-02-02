module Message where

import Control.Concurrent.STM
import Core

type Message = (Identifier, Action)

data Request = Request Message Response

type Response = TMVar String

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
tell :: Response -> String -> IO ()
tell sender message = atomically $ putTMVar sender message

-- Writes a message to the channel and waits for a response.
ask :: TChan Request -> Message -> IO String
ask chan message = do
  sender <- newEmptyTMVarIO
  atomically $ writeTChan chan $ Request message sender
  atomically $ takeTMVar sender

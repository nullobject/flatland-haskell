module Message where

import Control.Concurrent.STM
import Core
import Identifier

type Message = (Identifier, Action)

data Request a b = Request {
  payload :: b,
  sender  :: Response a
}

type Response a = TMVar a

type RequestChannel a b = TChan (Request a b)

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
drain :: RequestChannel a b -> IO [Request a b]
drain chan = atomically $ drainTChan chan

-- Replies to a message.
tell :: Response a -> a -> IO ()
tell sender message = atomically $ putTMVar sender message

-- Writes a message to the channel and waits for a response.
ask :: RequestChannel a b -> b -> IO a
ask chan message = do
  sender <- newEmptyTMVarIO
  atomically $ writeTChan chan $ Request message sender
  atomically $ takeTMVar sender

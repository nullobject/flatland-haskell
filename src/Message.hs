module Message where

import Control.Concurrent.STM
import Core
import Identifier

type Message = (Identifier, Action)

data Request a b = Request {
  payload :: a,
  sender  :: Response b
}

type Response b = TMVar b

type RequestChannel a b = TChan (Request a b)

drainTChan :: TChan b -> STM [b]
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

-- Replies to a request.
tell :: Response b -> b -> IO ()
tell sender payload = atomically $ putTMVar sender payload

-- Writes a request to the channel and waits for a response.
ask :: RequestChannel a b -> a -> IO b
ask chan payload = do
  sender <- newEmptyTMVarIO
  atomically $ writeTChan chan $ Request payload sender
  atomically $ takeTMVar sender

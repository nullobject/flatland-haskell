module Request where

import Control.Concurrent.STM

type Channel p s = TChan (Request p s)

data Request p s = Request {
  payload :: p,
  sender  :: Sender s
}

type Sender s = TMVar s

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
drain :: Channel p s -> IO [Request p s]
drain chan = atomically $ drainTChan chan

-- Replies to a request.
tell :: Sender s -> s -> IO ()
tell sender payload = atomically $ putTMVar sender payload

-- Writes a request to the channel and waits for a response.
ask :: Channel p s -> p -> IO s
ask chan payload = do
  sender <- newEmptyTMVarIO
  let request = Request {Request.payload = payload, Request.sender = sender}
  atomically $ writeTChan chan request
  atomically $ takeTMVar sender

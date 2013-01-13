module Game where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM
import           Control.Monad.State
import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as Map
import qualified Player
import qualified World
import qualified WorldView

oneSecond :: Int
oneSecond = 1000000

drainTChan :: TChan a -> STM [a]
drainTChan chan = do
  empty <- isEmptyTChan chan
  if empty
  then return []
  else do
    x <- readTChan chan
    xs <- drainTChan chan
    return (x:xs)

tell :: TMVar B.ByteString -> World.World -> STM ()
tell sender world = putTMVar sender $ encode $ WorldView.fromWorld world

initWorld :: World.WorldState ()
initWorld = World.spawn Player.empty

tick :: World.WorldState ()
tick = do
  world <- get
  let uuid = head $ Map.keys $ World.players world
  World.move uuid
  World.tick

run :: TChan (TMVar B.ByteString) -> IO ()
run chan = do
  world <- execStateT initWorld World.empty
  run' chan world

run' :: TChan (TMVar B.ByteString) -> World.World -> IO ()
run' chan world = do
  atomically $ do
    senders <- drainTChan chan
    mapM (\sender -> tell sender world) senders
  world' <- execStateT tick world
  threadDelay oneSecond
  run' chan world'

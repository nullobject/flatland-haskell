module Game where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM (atomically, STM)
import           Control.Monad.State
import qualified Data.Map as Map
import           Data.UUID.V4 (nextRandom)
import           GHC.Conc (unsafeIOToSTM)
import qualified Player
import qualified World
import qualified WorldView
import           Types

initWorld :: World.WorldState ()
initWorld = do
  uuid <- liftIO $ nextRandom
  World.spawn $ Player.empty uuid

tick :: World.WorldState ()
tick = do
  world <- get
  let uuid = head $ Map.keys $ World.players world
  World.move uuid
  World.tick

requestHandler :: World.World -> Request -> STM ()
requestHandler world (Request sender action) = do
  unsafeIOToSTM $ print action
  sender `tell` message
  where message = WorldViewMessage $ WorldView.fromWorld $ world

run :: RequestChan -> IO ()
run chan = do
  world <- execStateT initWorld World.empty
  run' chan world

run' :: RequestChan -> World.World -> IO ()
run' chan world = do
  atomically $ do
    requests <- drainTChan chan
    mapM (requestHandler world) requests
  world' <- execStateT tick world
  threadDelay oneSecond
  run' chan world'

module Game where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM (atomically)
import           Control.Monad.State
import qualified Data.Map as Map
import           Data.UUID (UUID)
import           Data.UUID.V4 (nextRandom)
import           GHC.Conc (unsafeIOToSTM)
import qualified Player
import           World (World, WorldState)
import qualified World
import qualified WorldView
import           Types

initWorld :: WorldState ()
initWorld = do
  uuid <- liftIO $ nextRandom
  World.spawn $ Player.empty uuid

execute :: Action -> UUID -> WorldState ()
execute Idle uuid = return ()
execute Move uuid = World.move uuid

handleRequest :: World -> Request -> IO World
handleRequest world (Request sender (ActionMessage action uuid)) = do
  putStrLn $ show action ++ " (" ++ show uuid ++ ")"
  world' <- execStateT (execute action uuid) world
  let message = WorldViewMessage $ WorldView.fromWorld $ world'
  sender `tell` message
  return world'

tick :: RequestChan -> World -> IO ()
tick chan world = do
  threadDelay oneSecond
  requests <- atomically $ drainTChan chan
  world' <- execStateT World.tick world
  world'' <- foldM handleRequest world' requests
  tick chan world''

run :: RequestChan -> IO ()
run chan = execStateT initWorld World.empty >>= tick chan

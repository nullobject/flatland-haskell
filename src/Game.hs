module Game where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM (TChan)
import           Control.Monad.State
import           Data.UUID (UUID)
import           Data.UUID.V4 (nextRandom)
import qualified Player
import           World (World, WorldState)
import qualified World
import qualified WorldView
import           Types

oneSecond :: Int
oneSecond = 1000000

initWorld :: WorldState ()
initWorld = do
  uuid <- liftIO $ nextRandom
  World.spawn $ Player.empty uuid

execute :: Action -> UUID -> WorldState ()
execute Move uuid = World.move uuid
execute _ _ = return ()

handleRequest :: World -> GameRequest -> IO World
handleRequest world (GameRequest sender (ActionMessage action uuid)) = do
  putStrLn $ show action ++ " (" ++ show uuid ++ ")"
  world' <- execStateT (execute action uuid) world
  let message = WorldViewMessage $ WorldView.fromWorld $ world'
  sender `tell` message
  return world'

tick :: TChan GameRequest -> World -> IO ()
tick chan world = do
  threadDelay oneSecond
  requests <- drain chan
  world' <- execStateT World.tick world
  world'' <- foldM handleRequest world' requests
  tick chan world''

run :: TChan GameRequest -> IO ()
run chan = execStateT initWorld World.empty >>= tick chan

module Game where

import           Control.Concurrent (threadDelay)
import           Control.Wire
import           Core
import           Prelude hiding ((.), id)
import           Request
import           World (World)
import qualified World
import           WorldView (WorldView)
import qualified WorldView

oneSecond :: Int
oneSecond = 1000000

-- Responds to the requests with the current world state.
--
-- TODO: scope the world view to the entity.
respond :: World -> [Request Message WorldView] -> IO ()
respond world = do
  mapM_ respond'
  where
    respond' request = do
      let worldView = WorldView.fromWorld world
      (Request.sender request) `tell` worldView

run :: Channel Message WorldView -> IO ()
run chan = do
  run' wire clockSession
  where
    wire = World.worldWire World.empty
    run' wire session = do
      threadDelay oneSecond
      requests <- drain chan
      let messages = map Request.payload requests
      (output, wire', session') <- stepSessionP wire session messages
      case output of
        Left x -> putStrLn $ "Inhibited: " ++ show x
        Right world -> do
          putStrLn $ "Produced: " ++ show world
          respond world requests
          run' wire' session'

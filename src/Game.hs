module Game where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM (TChan)
import           Control.Wire
import           Core
import           Message
import           Prelude hiding ((.), id)
import qualified World
import qualified WorldView

oneSecond :: Int
oneSecond = 1000000

run :: TChan Request -> IO ()
run chan = do
  run' (World.worldWire World.empty) clockSession
  where
    run' wire session = do
      threadDelay oneSecond
      requests <- drain chan
      let messages = map (\(Request message _) -> message) requests
      (output, wire', session') <- stepSessionP wire session messages
      case output of
        Left x -> putStrLn $ "Inhibited: " ++ show x
        Right world -> do
          putStrLn $ "Produced: " ++ show world
          let worldView = WorldView.fromWorld world
          mapM (\(Request _ sender) -> sender `tell` (show worldView)) requests
          run' wire' session'

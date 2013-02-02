module Game where

import           Control.Concurrent (threadDelay)
import           Control.Wire
import           Core
import           Message
import           Prelude hiding ((.), id)
import           World (World)
import qualified World
import qualified WorldView

oneSecond :: Int
oneSecond = 1000000

-- Responds to the requests with the current world state.
respond :: World -> [Request String] -> IO ()
respond world = do
  mapM_ respond'
  where
    respond' request = do
      let sender = Message.sender request
      let worldView = WorldView.fromWorld world
      sender `tell` show worldView

run :: RequestChannel String -> IO ()
run chan = do
  run' wire clockSession
  where
    wire = World.worldWire World.empty
    run' wire session = do
      threadDelay oneSecond
      requests <- drain chan
      let messages = map Message.payload requests
      (output, wire', session') <- stepSessionP wire session messages
      case output of
        Left x -> putStrLn $ "Inhibited: " ++ show x
        Right world -> do
          putStrLn $ "Produced: " ++ show world
          respond world requests
          run' wire' session'

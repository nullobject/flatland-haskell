module Game where

import           Channel
import           Control.Concurrent (threadDelay)
import           Control.Wire
import           Core
import qualified Data.Maybe as Maybe
import           Prelude hiding ((.), id)
import           World (World)
import qualified World
import           WorldView (WorldView)
import qualified WorldView

oneSecond :: Int
oneSecond = 1000000

-- Responds to the requests with the current world state.
respond :: World -> [Request Message WorldView] -> IO ()
respond world = mapM_ respond'
  where
    respond' (Request (identifier, _) sender) = do
      let player = Maybe.fromJust $ World.getPlayer identifier world
      let worldView = WorldView.forPlayer player world
      sender `tell` worldView

-- TODO: handle the case where the world wire inhibits.
run :: Channel Message WorldView -> IO ()
run chan = run' wire clockSession
  where
    wire = World.worldWire World.empty
    run' wire session = do
      threadDelay oneSecond
      requests <- drain chan
      let messages = map Channel.payload requests
      (output, wire', session') <- stepSession wire session messages
      case output of
        Left x -> putStrLn $ "Inhibited: " ++ show x
        Right world -> do
          putStrLn $ "Produced: " ++ show world
          respond world requests
          run' wire' session'

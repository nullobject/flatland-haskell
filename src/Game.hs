module Game where

import           Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import           Channel
import           Control.Concurrent (threadDelay, writeChan, Chan)
import           Control.Monad.State
import           Control.Wire
import           Core
import           Data.Aeson
import qualified Data.Maybe as Maybe
import           Map
import qualified Network.Wai.EventSource as Wai.EventSource
import           Prelude hiding ((.), id)
import           World
import           WorldView

oneSecond :: Int
oneSecond = 1000000

-- Responds to the requests with the current world state.
respond :: World -> [Request Message WorldView] -> IO ()
respond world = mapM_ respond'
  where
    respond' (Request (identifier, _) sender) = do
      let player = Maybe.fromJust $ getPlayer identifier world
      let worldView = newWorldView player world
      sender `tell` worldView

-- Runs the main game loop.
run :: Channel Message WorldView -> Chan Wai.EventSource.ServerEvent -> IO ()
run messageChannel eventChannel = do
  -- Load the tiled map.
  tiledMap <- loadMapFile "static/test.tmx"

  -- Create a world wire for the tiled map.
  let wire = worldWire $ newWorld tiledMap

  -- Run the loop.
  loop wire clockSession

  where
    loop wire session = do
      -- Sleep for a second.
      threadDelay oneSecond

      -- Get the pending messages.
      requests <- drain messageChannel
      let messages = map Channel.payload requests

      -- Step the world wire.
      (output, wire', session') <- stepSession wire session messages

      case output of
        -- TODO: handle the case where the world wire inhibits.
        Left x -> putStrLn $ "Inhibited: " ++ show x

        Right world -> do
          putStrLn $ "Produced: " ++ show world
          respond world requests
          _ <- liftIO $ writeChan eventChannel (Wai.EventSource.ServerEvent Nothing Nothing [fromLazyByteString $ encode world])
          loop wire' session'

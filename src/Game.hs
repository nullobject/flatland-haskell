module Game where

import           Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import           Channel
import           Control.Concurrent (threadDelay, writeChan, Chan)
import           Control.Monad.State
import           Control.Wire
import           Core
import           Data.Aeson (encode, ToJSON)
import qualified Data.Maybe as Maybe
import qualified Network.Wai.EventSource as Wai.EventSource
import           Prelude hiding ((.), id)
import           World (World)
import qualified World
import           WorldView (WorldView)
import qualified WorldView

oneSecond :: Int
oneSecond = 1000000

tiles = [ 0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41
        , 0x41, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x41
        , 0x41, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x41
        , 0x41, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x41
        , 0x41, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x41
        , 0x41, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x41
        , 0x41, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x41
        , 0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41 ]

-- Responds to the requests with the current world state.
respond :: World -> [Request Message WorldView] -> IO ()
respond world = mapM_ respond'
  where
    respond' (Request (identifier, _) sender) = do
      let player = Maybe.fromJust $ World.getPlayer identifier world
      let worldView = WorldView.forPlayer player world
      sender `tell` worldView

-- TODO: handle the case where the world wire inhibits.
run :: Channel Message WorldView -> Chan Wai.EventSource.ServerEvent -> IO ()
run messageChannel eventChannel = run' wire clockSession
  where
    wire = World.worldWire $ World.empty tiles
    run' wire session = do
      threadDelay oneSecond
      requests <- drain messageChannel
      let messages = map Channel.payload requests
      (output, wire', session') <- stepSession wire session messages
      case output of
        Left x -> putStrLn $ "Inhibited: " ++ show x
        Right world -> do
          putStrLn $ "Produced: " ++ show world
          respond world requests
          _ <- liftIO $ writeChan eventChannel (Wai.EventSource.ServerEvent Nothing Nothing [fromLazyByteString $ encode world])
          run' wire' session'

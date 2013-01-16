module Game where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM (TChan)
import           Control.Monad (forever)
import           Control.Monad.State (execStateT, foldM, get, liftIO, put, StateT)
import           Data.UUID (UUID)
import           Data.UUID.V4 (nextRandom)
import qualified Player
import           World (World, WorldState)
import qualified World
import qualified WorldView
import           Types

data Game = Game {
  chan  :: TChan GameRequest,
  world :: World
}

type GameState = StateT Game IO

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

-- Ticks the game.
tick :: GameState ()
tick = do
  game <- get
  world' <- liftIO $ threadDelay oneSecond >>
                     drain (chan game) >>=
                     foldM handleRequest (world game) >>=
                     execStateT World.tick
  put $ game {world = world'}

-- Runs the game with the given request channel.
run :: TChan GameRequest -> IO ()
run chan = do
  world <- execStateT initWorld World.empty
  loop $ Game chan world
  where loop game = execStateT (forever $ tick) game >> return ()

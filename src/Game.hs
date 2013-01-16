module Game where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM (TChan)
import           Control.Monad (forever)
import           Control.Monad.State
import           Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
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

-- Executes the request on the world.
executeRequest :: GameRequest -> WorldState ()
executeRequest (GameRequest sender (ActionMessage action uuid)) = executeAction action uuid
  where
    executeAction Idle uuid = do
      uuid <- liftIO $ UUID.nextRandom
      World.spawn $ Player.empty uuid
    executeAction Move uuid = World.move uuid
    executeAction _ _ = return ()

-- Executes the requests on the world.
executeRequests :: [GameRequest] -> GameState ()
executeRequests requests = do
  game <- get
  let actions = sequence $ map executeRequest requests
  world' <- liftIO $ execStateT actions $ world game
  put $ game {world = world'}

-- Replies to the requests with the world view.
replyToRequests :: [GameRequest] -> GameState ()
replyToRequests requests = do
  game <- get
  let message = WorldViewMessage $ WorldView.fromWorld $ world game
  liftIO $ mapM (replyToRequest message) requests
  return ()
  where replyToRequest message (GameRequest sender _) = sender `tell` message

-- Ticks the world.
tickWorld :: GameState ()
tickWorld = do
  game <- get
  world' <- liftIO $ execStateT World.tick $ world game
  put $ game {world = world'}

-- Ticks the game.
tick :: GameState ()
tick = do
  liftIO $ threadDelay oneSecond
  requests <- getRequests
  executeRequests requests
  tickWorld
  replyToRequests requests
  where getRequests = liftM chan get >>= liftIO . drain

-- Runs the game with the given request channel.
run :: TChan GameRequest -> IO ()
run chan = loop (Game chan World.empty) >> return ()
  where loop = execStateT $ forever $ tick

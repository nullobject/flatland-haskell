module Game where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM (TChan)
import           Control.Monad.State
import           Core
import qualified Data.UUID.V4 as UUID
import qualified Entity
import           Message
import           World (World, WorldState)
import qualified World
import qualified WorldView

data GameState = GameState {
  chan  :: TChan Request,
  world :: WorldState
}

type Game = StateT GameState IO

oneSecond :: Int
oneSecond = 1000000

-- Executes the request on the world.
-- TODO: only spawn a entity if they aren't already in the world.
executeRequest :: Request -> World ()
executeRequest (Request (ActionMessage action identifier) _) = executeAction action
  where
    executeAction Idle = do
      identifier' <- liftIO UUID.nextRandom
      World.spawn $ Entity.empty (Identifier identifier')
    executeAction Move = World.move identifier
    executeAction _ = return ()

-- Executes the requests on the world.
executeRequests :: [Request] -> Game ()
executeRequests requests = do
  game <- get
  let actions = sequence $ map executeRequest requests
  world' <- liftIO $ execStateT actions $ world game
  put $ game {world = world'}

-- Replies to the requests with the world view.
replyToRequests :: [Request] -> Game ()
replyToRequests requests = do
  game <- get
  let message = WorldViewMessage $ WorldView.fromWorld $ world game
  _ <- liftIO $ mapM (replyToRequest message) requests
  return ()
  where replyToRequest message (Request _ sender) = sender `tell` message

-- Ticks the world.
tickWorld :: Game ()
tickWorld = do
  game <- get
  world' <- liftIO $ execStateT World.tick $ world game
  put $ game {world = world'}

-- Ticks the game.
tick :: Game ()
tick = do
  liftIO $ threadDelay oneSecond
  requests <- getRequests
  executeRequests requests
  tickWorld
  replyToRequests requests
  where getRequests = liftM chan get >>= liftIO . drain

-- Runs the game with the given request channel.
run :: TChan Request -> IO ()
run chan' = loop (GameState chan' World.empty) >> return ()
  where loop = execStateT $ forever $ tick

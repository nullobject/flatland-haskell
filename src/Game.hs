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

transformRequest :: GameRequest -> WorldState ()
transformRequest (GameRequest sender (ActionMessage action uuid)) = execute action uuid

transformRequests :: [GameRequest] -> [WorldState ()]
transformRequests requests = map transformRequest requests

replyToRequest :: Message -> GameRequest -> IO ()
replyToRequest message (GameRequest sender _) = do
  sender `tell` message

replyToRequests :: [GameRequest] -> GameState ()
replyToRequests requests = do
  game <- get
  let message = WorldViewMessage $ WorldView.fromWorld $ world game
  liftIO $ mapM (replyToRequest message) requests
  return ()

applyRequests :: [GameRequest] -> GameState ()
applyRequests requests = do
  game <- get
  let actions = sequence $ transformRequests requests
  world' <- liftIO $ execStateT actions $ world game
  put $ game {world = world'}

tickWorld :: GameState ()
tickWorld = do
  game <- get
  world' <- liftIO $ execStateT World.tick $ world game
  put $ game {world = world'}

getRequests :: GameState ([GameRequest])
getRequests = do
  game <- get
  liftIO $ drain $ chan game

-- Ticks the game.
tick :: GameState ()
tick = do
  liftIO $ threadDelay oneSecond
  requests <- getRequests
  applyRequests requests
  tickWorld
  replyToRequests requests

-- Runs the game with the given request channel.
run :: TChan GameRequest -> IO ()
run chan = do
  world <- execStateT initWorld World.empty
  loop $ Game chan world
  where loop game = execStateT (forever $ tick) game >> return ()

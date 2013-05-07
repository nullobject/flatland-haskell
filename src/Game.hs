module Game where

import           Action
import           Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import           Channel
import           Control.Concurrent (threadDelay, writeChan, Chan)
import           Control.Monad.State
import           Control.Wire
import           Core
import           Data.Aeson
import qualified Data.Map as Map
import           Data.Maybe
import           Entity
import           Identifier
import           Map
import qualified Network.Wai.EventSource as Wai.EventSource
import           Player
import           Prelude hiding ((.), id)
import           World
import           WorldView

oneSecond :: Int
oneSecond = 1000000

-- Represents the state of the game.
data Game = Game
  {
    -- The game world.
    gameWorld :: World

    -- A map of the players in the game.
  , gamePlayersMap :: PlayersMap
  } deriving (Show)

-- A game wire takes a list of messages and produces a game state.
type GameWire = MyWire [Message] Game

-- Returns the players in the game.
gamePlayers :: Game -> [Player]
gamePlayers game = Map.elems $ gamePlayersMap game

-- Returns the player in the game with the given identifier.
playerWithId :: Identifier -> Game -> Maybe Player
playerWithId identifier game = Map.lookup identifier playersMap
  where playersMap = gamePlayersMap game

-- Returns a new game state for the given tiled map.
newGame :: TiledMap -> Game
newGame tiledMap = Game { gameWorld      = world
                        , gamePlayersMap = gamePlayers
                        }

  where world       = newWorld tiledMap
        gamePlayers = Map.empty

-- Returns a new game wire for the given initial game state.
gameWire :: Game -> GameWire
gameWire game = proc playerMessages -> do
  -- Step the player router.
  (entityMessages, playersMap) <- playerRouter playersMap0 playerWire -< playerMessages

  -- Step the world wire.
  world <- worldWire world0 -< entityMessages

  -- Return a new game state.
  returnA -< game { gameWorld      = world
                  , gamePlayersMap = playersMap
                  }

  where world0      = gameWorld game
        playersMap0 = Map.empty

-- Responds to the given requests with the game state.
respond :: [Request Message WorldView] -> Game -> IO ()
respond requests game = mapM_ respond' requests
  where
    respond' (Request (identifier, _) sender) = do
      let player    = fromJust $ playerWithId identifier game
          worldView = newWorldView player $ gameWorld game
      sender `tell` worldView

-- Runs the main game loop.
run :: Channel Message WorldView -> Chan Wai.EventSource.ServerEvent -> IO ()
run messageChannel eventChannel = do
  -- Load the tiled map.
  tiledMap <- loadMapFile "static/test.tmx"

  -- Create a game wire for the tile map.
  let wire = gameWire $ newGame tiledMap

  -- Run the loop.
  loop wire $ counterSession 1

  where
    loop :: GameWire -> Session IO -> IO ()
    loop wire session = do
      -- Drain the pending messages.
      requests <- drain messageChannel
      let messages = map Channel.payload requests

      -- Step the game wire.
      (mx, wire', session') <- stepSession wire session messages

      -- Respond to the requests.
      case mx of
        -- Left _ ->
        Right game -> do
          putStrLn $ show game
          respond requests game

      -- Sleep for a second.
      threadDelay oneSecond

      -- Recurse with the new wire and session.
      loop wire' session'

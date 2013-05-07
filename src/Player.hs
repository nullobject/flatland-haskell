module Player where

import           Action
import           Bullet
import           Control.Wire hiding (object)
import qualified Control.Wire as Wire
import           Core
import           Entity (Entity)
import qualified Entity
import           Data.Aeson
import           Data.Char (toLower)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Geometry
import           Identifier
import           Prelude hiding ((.), id)
import           Wire

-- The player state.
data State =
    Dead
  | Spawning
  | Alive
  deriving (Eq, Show)

instance ToJSON State where
  toJSON s = toJSON $ map toLower $ show s

-- Represents the state of a player.
data Player = Player
  {
    -- The player identifier.
    playerId :: Identifier

    -- The player state.
  , playerState :: State

    -- The identifier of the entity controlled by the player.
  , playerEntityId :: Maybe Identifier
  } deriving (Show)

instance ToJSON Player where
  toJSON player = object [ "id"       .= playerId       player
                         , "state"    .= playerState    player
                         , "entityId" .= playerEntityId player
                         ]

-- A player wire takes a message and produces a possible entity message and a
-- player state.
type PlayerWire = MyWire Message (Maybe Message, Player)

-- A map from identifiers to players.
type PlayersMap = Map Identifier Player

-- A player router is a wire which takes a list of player messages and produces
-- a list of entity messages and a players map.
type PlayerRouter = MyWire [Message] ([Message], PlayersMap)

-- A function which returns a new player wire for the given identifier.
type PlayerConstructor = Identifier -> PlayerWire

-- Returns a new player with the given identifier.
newPlayer :: Identifier -> Player
newPlayer identifier = Player { playerId       = identifier
                              , playerState    = Dead
                              , playerEntityId = Nothing
                              }

-- Returns a new player wire given an initial player state.
--
-- When a 'spawn' message is received the player enters the Spawning state.
-- After 3 seconds, an entity is spawned and the player enters the Alive state.
playerWire_ :: Player -> PlayerWire
playerWire_ player = proc playerMessage -> do
  -- TODO: pass through the player message only if an entity has been spawned.
  returnA -< (Just playerMessage, player)

playerWire :: PlayerConstructor
playerWire identifier = playerWire_ $ newPlayer identifier

-- Routes messages to players (defaults to the 'Idle' message if a message
-- doesn't exist for a player).
playerRouter_
  :: PlayersMap              -- ^ Map from identifiers to players.
  -> (Message -> Identifier) -- ^ Function to turn the message into an identifier.
  -> PlayerConstructor       -- ^ Base wire constructor.
  -> PlayerRouter
playerRouter_ playersMap key constructor = playerRouter_' playersMap $ context_ key constructor
  where
    playerRouter_' :: PlayersMap -> PlayerWire -> PlayerRouter
    playerRouter_' playersMap context = mkGen $ \dt messages -> do
      let messages' = ensurePlayerMessage messages $ Map.keys playersMap
      (playersMap', context') <- loop dt context playersMap messages'
      return (Right ([], playersMap'), playerRouter_' playersMap' context')

    -- Steps the player context with each message.
    loop :: Time -> PlayerWire -> PlayersMap -> [Message] -> IO (PlayersMap, PlayerWire)
    loop dt context playersMap [] = return (playersMap, context)
    loop dt context playersMap messages = do
      let message = head messages
      let messages' = tail messages
      (mx, context') <- stepWire context dt message
      let playersMap' = case mx of
                        Left _                        -> Map.delete (key message) playersMap
                        Right (entityMessage, player) -> Map.insert (key message) player playersMap
      loop dt context' playersMap' messages'

-- Returns a player router for the given entity constructor.
playerRouter :: PlayersMap -> PlayerConstructor -> PlayerRouter
playerRouter playersMap constructor = playerRouter_ playersMap messageId constructor

-- Ensures each identifier has an action (defaults to the 'Idle' action).
ensurePlayerMessage :: [Message] -> [Identifier] -> [Message]
ensurePlayerMessage messages identifiers = Map.toList actionsMap'
  where actionsMap = Map.fromList messages
        actionsMap' = foldl (\map identifier -> Map.alter update identifier map) actionsMap identifiers
        update (Just action) = Just action
        update Nothing       = Just Action.Idle

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
import qualified Data.Key as Key
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
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

-- A player wire takes a possible player message and produces a possible entity
-- message and a player state.
type PlayerWire = MyWire (Maybe Message) (Maybe Message, Player)

-- A map from identifiers to players.
type PlayerMap = Map Identifier Player

-- A map from identifiers to player wires.
type PlayerWireMap = Map Identifier PlayerWire

-- A player router is a wire which takes a list of player messages and produces
-- a list of entity messages and a players map.
type PlayerRouter = MyWire [Message] ([Message], PlayerMap)

-- A function which returns a new player wire for the given identifier.
type PlayerWireConstructor = Identifier -> PlayerWire

-- Returns a new player with the given identifier.
newPlayer :: Identifier -> Player
newPlayer identifier = Player { playerId       = identifier
                              , playerState    = Dead
                              , playerEntityId = Nothing
                              }

-- Returns a new player wire given an initial player state.
--
-- TODO: Pass through the player message only if an entity has been spawned.
-- TODO: If a player message hasn't been received for 10 seconds, then inhibit.
playerWire_ :: Player -> PlayerWire
playerWire_ player = proc playerMessage -> do
  returnA -< (playerMessage, player)

playerWire :: PlayerWireConstructor
playerWire identifier = playerWire_ $ newPlayer identifier

-- Returns a wire which routes messages to players.
playerRouter :: PlayerWireConstructor -> PlayerRouter
playerRouter playerWireConstructor = playerRouter' Map.empty
  where
    playerRouter' :: PlayerWireMap -> PlayerRouter
    playerRouter' playerWireMap = mkGen $ \dt messages -> do
      let messageMap     = mapMessages messages
          playerWireMap' = foldl ensurePlayerWire playerWireMap $ Map.keys messageMap

      (messages', playerMap, playerWireMap'') <- stepPlayerWireMap dt messageMap playerWireMap'

      return (Right (messages', playerMap), playerRouter' playerWireMap'')

    -- Ensures a player wire with the given identifier exists in the map. If a
    -- player wire does not exist, then it inserts one using the player wire
    -- constructor.
    ensurePlayerWire :: PlayerWireMap -> Identifier -> PlayerWireMap
    ensurePlayerWire playerWireMap identifier = Map.alter f identifier playerWireMap
      where f          = Just . maybe playerWire Wire.id
            playerWire = playerWireConstructor identifier

    -- Steps the player wire map with the given messages.
    stepPlayerWireMap :: Time -> MessageMap -> PlayerWireMap -> IO ([Message], PlayerMap, PlayerWireMap)
    stepPlayerWireMap dt messageMap playerWireMap = do
      -- Step the player wires with any pending messages.
      result <- Key.mapWithKeyM (\identifier wire -> stepWire wire dt (Map.lookup identifier messageMap)) playerWireMap

      -- Filter out inhibiting player wires.
      let result' = Map.filter (\(mx, _) -> isRight mx) result

      -- Extract the parts of the result.
      let messages       = Maybe.catMaybes (fmap Map.elems (fmap (fst . fromRight . fst)) result')
          playerMap      = fmap (snd . fromRight . fst) result'
          playerWireMap' = fmap snd result'

      return (messages, playerMap, playerWireMap')

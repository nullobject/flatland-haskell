{-# LANGUAGE DeriveGeneric, TupleSections #-}

module Player where

import           Action
import           Control.Wire
import qualified Control.Wire as Wire
import           Core
import           Entity (Entity)
import qualified Entity
import           Data.Aeson (toJSON, ToJSON)
import           Data.Char (toLower)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Traversable as Traversable
import qualified Data.Key as Key
import           GHC.Generics (Generic)
import           Identifier (Identifier)
import qualified Identifier
import           Prelude hiding ((.), id)

data State =
    Dead
  | Spawning
  | Alive
  deriving (Eq, Generic, Show)

instance ToJSON State where
  toJSON s = toJSON $ map toLower $ show s

-- | A player represents the state of a player in the game. A player controls
-- an entity in the world.
data Player = Player
  { id     :: Identifier
  , state  :: State
  , entity :: Maybe Entity
  } deriving (Generic, Show)

instance ToJSON Player

-- A player wire takes an action and produces a new player state.
type PlayerWire = MyWire Action Player

-- A map from an identifier to a player wire.
type PlayerWireMap = Map Identifier PlayerWire

empty :: Identifier -> Player
empty identifier = Player
  { Player.id = identifier
  , state     = Dead
  , entity    = Nothing
  }

-- Spawns a new entity.
spawnEntityWire :: MyWire Action (Maybe Entity)
spawnEntityWire = mkGen $ \dt action -> do
  identifier <- Identifier.nextRandom
  let wire = Entity.entityWire $ Entity.empty identifier
  stepWire wire dt action

-- Returns a new player wire given an initial player state.
--
-- When a 'spawn' message is received the player enters the Spawning state.
-- After 3 seconds, an entity is spawned and the player enters the Alive state.
playerWire :: Player -> PlayerWire
playerWire player = proc action -> do
  (state', entity') <- continually $ entityWire -< action
  returnA -< player {state = state', entity = entity'}
  where entityWire = pure (Dead, Nothing) . Wire.until (== Spawn) -->
                     pure (Spawning, Nothing) . for 3 -->
                     pure Alive &&& spawnEntityWire

-- Evolves a list of player wires, routing actions which are addressed to them
-- by matching their identifiers. Actions which are addressed to unknown player
-- wires are created using the constructor.
routeWire :: (Identifier -> PlayerWire) -> MyWire [Message] [Player]
routeWire constructor = route Map.empty
  where
    route :: PlayerWireMap -> MyWire [Message] [Player]
    route playerWireMap = mkGen $ \dt messages -> do
      -- Create a map from identifiers to actions.
      let actionMap = Map.fromList messages

      -- Ensure the messages can be delivered to player wires.
      let playerWireMap' = foldl spawn playerWireMap $ Map.keys actionMap

      -- Step the player wires, supplying the optional actions.
      res <- Key.mapWithKeyM (\identifier wire -> stepWire wire dt (Map.findWithDefault Action.Idle identifier actionMap)) playerWireMap'

      -- WTF does this do?
      let resx = Traversable.sequence . fmap (\(mx, w) -> fmap (, w) mx) $ res

      return (fmap Map.elems (fmap (fmap fst) resx), route (fmap snd res))

    -- Spawns a new player wire if one with the identifier doesn't already exist.
    spawn :: PlayerWireMap -> Identifier -> PlayerWireMap
    spawn playerWireMap identifier = Map.alter f identifier playerWireMap
      where
        f = Just . maybe wire Wire.id
        wire = constructor identifier

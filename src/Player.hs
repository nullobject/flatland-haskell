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
import qualified Data.Maybe as Maybe
import qualified Data.Traversable as Traversable
import qualified Data.Key as Key
import           GHC.Generics (Generic)
import           Identifier
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
  , entity :: Maybe Entity
  , state  :: State
  } deriving (Generic, Show)

instance ToJSON Player

-- A player wire takes a message and produces a new player state.
type PlayerWire = WireP (Maybe Action) Player

-- A map from an identifier to a player wire.
type PlayerWireMap = Map Identifier PlayerWire

empty :: Identifier -> Player
empty identifier = Player
  { Player.id = identifier
  , entity    = Nothing
  , state     = Dead
  }

-- TODO: Should return spawning when spawning an entity.
stateWire :: WireP (Maybe Entity) State
stateWire = pure Alive . when Maybe.isJust <|> pure Dead

-- Waits for a spawn action, then waits for 3 seconds, and then inhibits.
waitForSpawnWire :: WireP (Maybe Action) (Maybe Entity)
waitForSpawnWire = pure Nothing . (Wire.until (== Just Spawn) --> for 3)

-- Evolves an entity.
entityWire :: Identifier -> WireP (Maybe Action) (Maybe Entity)
entityWire identifier = proc action -> do
  entity' <- Entity.entityWire $ Entity.empty identifier -< action
  _ <- deadWire -< entity'
  returnA -< Just entity'

-- Inhibits if the entity is dead.
deadWire :: WireP Entity Entity
deadWire = Wire.until dead --> Wire.empty
  where dead entity = Entity.state entity == Entity.Dead

-- Returns a new player wire given an initial player state.
--
-- The player wire controls the players' state and the state of the player's
-- entity.
playerWire :: Player -> PlayerWire
playerWire player = proc action -> do
  entity' <- continually $ waitForSpawnWire --> entityWire identifier -< action
  state' <- stateWire -< entity'
  returnA -< player {entity = entity', state = state'}
  where identifier = Player.id player

-- Evolves a list of player wires, routing actions which are addressed to them
-- by matching their identifiers. Actions which are addressed to unknown player
-- wires are created using the constructor.
routeWire :: (Identifier -> PlayerWire) -> WireP [Message] [Player]
routeWire constructor = route Map.empty
  where
    route :: PlayerWireMap -> WireP [Message] [Player]
    route playerWireMap = mkGen $ \dt messages -> do
      -- Create a map from identifiers to actions.
      let actionMap = Map.fromList messages

      -- Ensure the messages can be delivered to player wires.
      let playerWireMap' = foldl spawn playerWireMap $ Map.keys actionMap

      -- Step the player wires, supplying the optional actions.
      res <- Key.mapWithKeyM (\identifier wire -> stepWire wire dt (Map.lookup identifier actionMap)) playerWireMap'

      -- WTF does this do?
      let resx = Traversable.sequence . fmap (\(mx, w) -> fmap (, w) mx) $ res

      return (fmap Map.elems (fmap (fmap fst) resx), route (fmap snd res))

    -- Spawns a new player wire if one with the identifier doesn't already exist.
    spawn :: PlayerWireMap -> Identifier -> PlayerWireMap
    spawn playerWireMap identifier = Map.alter f identifier playerWireMap
      where
        f = Just . maybe wire Wire.id
        wire = constructor identifier

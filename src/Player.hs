{-# LANGUAGE TupleSections #-}

module Player where

import           Action
import           Bullet
import           Collision (AABB)
import           Control.Wire hiding (object)
import qualified Control.Wire as Wire
import           Core
import           Entity (Entity, EntityWire)
import qualified Entity
import           Data.Aeson
import           Data.Char (toLower)
import qualified Data.Key as Key
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Traversable as Traversable
import           Geometry (Rectangle)
import           Identifier
import           Prelude hiding ((.), id)

data State =
    Dead
  | Spawning
  | Alive
  deriving (Eq, Show)

instance ToJSON State where
  toJSON s = toJSON $ map toLower $ show s

-- | A player represents the state of a player in the game. A player controls
-- an entity in the world.
data Player = Player
  { playerId     :: Identifier
  , playerState  :: State
  , playerEntity :: Maybe Entity
  } deriving (Show)

instance ToJSON Player where
  toJSON player = object [ "id"     .= playerId     player
                         , "state"  .= playerState  player
                         , "entity" .= playerEntity player ]

type RouteWire = MyWire ([AABB], [Message]) [(Player, Maybe Bullet)]

-- A player wire takes an action and produces a player state and a possible
-- bullet state.
type PlayerWire = MyWire ([AABB], Action) (Player, Maybe Bullet)

-- A map from an identifier to a player wire.
type PlayerWireMap = Map Identifier PlayerWire

emptyPlayer :: Identifier -> Player
emptyPlayer identifier = Player
  { playerId     = identifier
  , playerState  = Dead
  , playerEntity = Nothing
  }

-- Returns a new player wire given an initial player state.
--
-- When a 'spawn' message is received the player enters the Spawning state.
-- After 3 seconds, an entity is spawned and the player enters the Alive state.
playerWire :: [Rectangle] -> Player -> PlayerWire
playerWire spawnRectangles player = proc (objects, action) -> do
  (state', (entity', bullet')) <- continually $ entityWire -< (objects, action)

  returnA -< ( player { playerState  = state'
                      , playerEntity = entity'}
             , bullet' )

  where entityWire = pure (Dead, (Nothing, Nothing)) . Wire.until (\(objects, action) -> action == Spawn) -->
                     pure (Spawning, (Nothing, Nothing)) . for 3 -->
                     pure Alive &&& Entity.spawnWire spawnRectangles

-- Evolves a list of player wires, routing actions which are addressed to them
-- by matching their identifiers. Actions which are addressed to unknown player
-- wires are created using the constructor.
--
-- TODO: Refactor this function.
routeWire :: (Identifier -> PlayerWire) -> RouteWire
routeWire constructor = route Map.empty
  where
    route :: PlayerWireMap -> RouteWire
    route playerWireMap = mkGen $ \dt (objects, messages) -> do
      -- Create a map from identifiers to actions.
      let actionMap = Map.fromList messages

      -- Ensure the messages can be delivered to player wires.
      let playerWireMap' = foldl spawn playerWireMap $ Map.keys actionMap

      -- Step the player wires, supplying the optional actions.
      res <- Key.mapWithKeyM (\identifier wire -> stepWire wire dt (objects, Map.findWithDefault Action.Idle identifier actionMap)) playerWireMap'

      -- WTF does this do?
      let res' = Traversable.sequence . fmap (\(mx, w) -> fmap (, w) mx) $ res

      return (fmap Map.elems (fmap (fmap fst) res'), route (fmap snd res))

    -- Spawns a new player wire if one with the identifier doesn't already exist.
    spawn :: PlayerWireMap -> Identifier -> PlayerWireMap
    spawn playerWireMap identifier = Map.alter f identifier playerWireMap
      where
        f = Just . maybe wire Wire.id
        wire = constructor identifier

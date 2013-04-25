module Player where

import           Action
import           Bullet
import           Collision (AABB)
import           Control.Wire hiding (object)
import qualified Control.Wire as Wire
import           Core
import           Entity (Entity)
import qualified Entity
import           Data.Aeson
import           Data.Char (toLower)
import           Geometry
import           Identifier
import           Prelude hiding ((.), id)

-- The player state.
data State =
    Dead
  | Spawning
  | Alive
  deriving (Eq, Show)

instance ToJSON State where
  toJSON s = toJSON $ map toLower $ show s

-- Represents the state of a player in the game.
data Player = Player
  {
    -- The player identifier.
    playerId :: Identifier

    -- The player state.
  , playerState :: State

    -- The entity controlled by the player.
  , playerEntity :: Maybe Entity
  } deriving (Show)

instance ToJSON Player where
  toJSON player = object [ "id"     .= playerId     player
                         , "state"  .= playerState  player
                         , "entity" .= playerEntity player
                         ]

-- A player wire takes an action and produces a player state and a possible
-- bullet state.
type PlayerWire = MyWire ([AABB], Action) (Player, Maybe Bullet)

-- Returns a new player with the given identifier.
newPlayer :: Identifier -> Player
newPlayer identifier = Player
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
  (state, (entity, bullet)) <- continually $ entityWire -< (objects, action)

  returnA -< (player {playerState = state, playerEntity = entity}, bullet)

  where entityWire = pure (Dead, (Nothing, Nothing)) . Wire.until (\(objects, action) -> action == Spawn) -->
                     pure (Spawning, (Nothing, Nothing)) . for 3 -->
                     pure Alive &&& Entity.spawnWire spawnRectangles

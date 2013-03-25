{-# LANGUAGE DeriveGeneric, TupleSections #-}

module Entity where

import           Action
import           Control.Wire
import qualified Control.Wire as Wire
import           Core
import           Data.Aeson (toJSON, ToJSON)
import           Data.Char (toLower)
import           Data.VectorSpace
import           Geometry (Point, Vector)
import           GHC.Generics (Generic)
import           Identifier
import           Prelude hiding ((.), id)

data State =
    Idle
  | Attacking
  | Moving
  | Turning
  deriving (Eq, Generic, Show)

instance ToJSON State where
  toJSON s = toJSON $ map toLower $ show s

-- An entity is an actor in the world.
data Entity = Entity
  { id        :: Identifier
  , state     :: State
  , age       :: Age
  , direction :: Direction
  , velocity  :: Vector
  , position  :: Point
  , health    :: Health
  , energy    :: Energy
  } deriving (Generic, Show)

instance ToJSON Entity

-- An entity wire takes an action and produces a new entity state.
type EntityWire = MyWire Action (Maybe Entity)

speed :: Double
speed = 1.0

-- Returns a new entity.
empty :: Identifier -> Entity
empty identifier = Entity
  { Entity.id = identifier
  , state     = Entity.Idle
  , age       = 0
  , direction = 0
  , velocity  = zeroV
  , position  = zeroV
  , health    = 100
  , energy    = 100
  }

-- If the entity has enough energy then it returns the updated energy value and
-- the action. Otherwise it returns the original energy value and defaults to
-- the idle action.
energyActionWire :: (Energy, Action) -> MyWire Action (Energy, Action)
energyActionWire = accum1 update
  where
    -- Returns a new energy value and action from the input action.
    update (energy, _) action = doAction energy $ calculateAction energy action

    -- Returns a new energy action pair.
    doAction energy action = (min 100 $ energy + cost action, action)

    -- Ensures the entity has enough energy to perform the action.
    calculateAction energy action = if energy + cost action >= 0
                                    then action
                                    else Action.Idle

directionWire :: Direction -> MyWire Action Direction
directionWire = accum1 update
  where update direction (Turn direction') = direction'
        update direction _                 = direction

-- The velocity wire returns the current velocity for the entity. It changes
-- the velocity when it receives a forward/reverse action.
--
-- TODO: Detect collisions with wall segments and other entities.
velocityWire :: Vector -> MyWire (Direction, Action) Point
velocityWire = accum1 update
  where update velocity (direction, Forward) = speed *^ (cos direction, sin direction)
        update velocity (direction, Reverse) = -speed *^ (cos direction, sin direction)
        update velocity _                    = zeroV

-- The health wire returns the current health of the entity. It inhibits when
-- the entity dies.
--
-- TODO: Health should depend on collisions with other entities.
healthWire :: Health -> MyWire Age Health
healthWire health0 = pure health0 . when (< 100000) <|> Wire.empty

stateWire :: MyWire Action State
stateWire = execute_ $ \action -> return $ case action of
  Attack   -> Entity.Attacking
  Forward  -> Entity.Moving
  Reverse  -> Entity.Moving
  (Turn _) -> Entity.Turning
  _        -> Entity.Idle

-- Returns a new entity wire given an initial entity state.
entityWire :: Entity -> EntityWire
entityWire entity = proc action -> do
  (energy', action') <- energyActionWire energyAction0 -< action
  state'             <- stateWire                      -< action'
  age'               <- countFrom age0                 -< 1
  direction'         <- directionWire direction0       -< action'
  velocity'          <- velocityWire velocity0         -< (direction', action')
  position'          <- integral1_ position0           -< velocity'
  health'            <- healthWire health0             -< age'

  returnA -< Just entity { state     = state'
                         , age       = age'
                         , direction = direction'
                         , velocity  = velocity'
                         , position  = position'
                         , health    = health'
                         , energy    = energy'
                         }

  where age0          = age entity
        direction0    = direction entity
        health0       = health entity
        velocity0     = velocity entity
        position0     = position entity
        energyAction0 = (energy entity, Action.Idle)

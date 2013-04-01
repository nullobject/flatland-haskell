{-# LANGUAGE DeriveGeneric, TupleSections #-}

module Entity where

import           Action
import           Collision
import           Control.Wire
import qualified Control.Wire as Wire
import           Core
import           Data.Aeson (toJSON, ToJSON)
import           Data.Char (toLower)
import           Data.VectorSpace
import qualified Data.Maybe as Maybe
import           Geometry
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
  , position  :: Position
  , velocity  :: Velocity
  , health    :: Health
  , energy    :: Energy
  } deriving (Generic, Show)

instance ToJSON Entity

-- An entity wire takes a list of AABBs and an action and produces a new entity
-- state.
type EntityWire = MyWire ([AABB], Action) (Maybe Entity)

speed :: Double
speed = 1

-- Returns a new entity.
empty :: Identifier -> Entity
empty identifier = Entity
  { Entity.id = identifier
  , state     = Entity.Idle
  , age       = 0
  , direction = 0
  , position  = zeroV
  , velocity  = zeroV
  , health    = 100
  , energy    = 100 }

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
impulseWire :: MyWire (Direction, Action) Velocity
impulseWire = execute_ $ return . update
  where update (direction, Forward) = vector direction
        update (direction, Reverse) = -(vector direction)
        update _                    = zeroV
        vector direction = (cos direction, sin direction) ^* speed

-- The health wire returns the current health of the entity. It inhibits when
-- the entity dies.
--
-- TODO: Health should depend on collisions with other entities/bullets.
healthWire :: Health -> MyWire Age Health
healthWire health0 = pure health0 . when (< 100000) <|> Wire.empty

stateWire :: MyWire Action State
stateWire = execute_ $ \action -> return $ case action of
  Attack   -> Entity.Attacking
  Forward  -> Entity.Moving
  Reverse  -> Entity.Moving
  (Turn _) -> Entity.Turning
  _        -> Entity.Idle

-- The collision wire takes a list of AABBs and an impulse velocity and outputs
-- the position, velocity, and contacts of the entity.
collisionWire :: (Position, Velocity) -> MyWire ([AABB], Velocity) (Position, Velocity, [Contact])
collisionWire (position0, velocity0) =
  mkPure $ \_ (objects, impulse) ->
    let (position, velocity, contacts) = collideWithObjects (objects, position0, impulse)
    in (Right (position, velocity, contacts), collisionWire (position, velocity))

-- Returns a new entity wire given an initial entity state.
entityWire :: Entity -> EntityWire
entityWire entity = proc (objects, action) -> do
  age'                              <- countFrom age0                       -< 1
  (energy', action')                <- energyActionWire (energy0, action0)  -< action
  state'                            <- stateWire                            -< action'
  direction'                        <- directionWire direction0             -< action'
  impulse'                          <- impulseWire                          -< (direction', action')
  (position', velocity', contacts') <- collisionWire (position0, velocity0) -< (objects, impulse')
  health'                           <- healthWire health0                   -< age'

  returnA -< Just entity { state     = state'
                         , age       = age'
                         , direction = direction'
                         , position  = position'
                         , velocity  = velocity'
                         , health    = health'
                         , energy    = energy' }

  where action0       = Action.Idle
        age0          = age entity
        direction0    = direction entity
        energy0       = energy entity
        health0       = health entity
        position0     = position entity
        velocity0     = velocity entity

module Entity where

import           Action
import           Bullet
import           Collision
import           Control.Wire hiding (object)
import qualified Control.Wire as Wire
import           Core
import           Data.Aeson
import           Data.Char (toLower)
import           Data.VectorSpace
import qualified Data.Maybe as Maybe
import           Geometry
import           Identifier
import           Prelude hiding ((.), id)

data State =
    Idle
  | Attacking
  | Moving
  | Turning
  deriving (Eq, Show)

instance ToJSON State where
  toJSON s = toJSON $ map toLower $ show s

-- An entity is an actor in the world.
data Entity = Entity
  { entityId        :: Identifier
  , entityState     :: State
  , entityAge       :: Age
  , entityDirection :: Direction
  , entityPosition  :: Position
  , entityVelocity  :: Velocity
  , entityHealth    :: Health
  , entityEnergy    :: Energy
  } deriving (Show)

instance ToJSON Entity where
  toJSON entity = object [ "id"        .= entityId        entity
                         , "state"     .= entityState     entity
                         , "age"       .= entityAge       entity
                         , "direction" .= entityDirection entity
                         , "position"  .= entityPosition  entity
                         , "velocity"  .= entityVelocity  entity
                         , "health"    .= entityHealth    entity
                         , "energy"    .= entityEnergy    entity ]

-- An entity wire takes a list of AABBs and an action and produces a new entity
-- state and a bullet.
type EntityWire = MyWire ([AABB], Action) (Maybe Entity, Maybe Bullet)

entitySpeed :: Double
entitySpeed = 1

bulletSpeed :: Double
bulletSpeed = 1

-- Returns a new entity.
emptyEntity :: Identifier -> Position -> Entity
emptyEntity identifier position = Entity
  { entityId        = identifier
  , entityState     = Entity.Idle
  , entityAge       = 0
  , entityDirection = 0
  , entityPosition  = position
  , entityVelocity  = zeroV
  , entityHealth    = 100
  , entityEnergy    = 100 }

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
        vector direction = (cos direction, sin direction) ^* entitySpeed

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
    let (position, velocity, contacts) = collideWithObjects objects position0 impulse
    in (Right (position, velocity, contacts), collisionWire (position, velocity))

-- The bullet wire produces a bullet moving in a direction from a position if
-- the player is attacking.
bulletWire :: MyWire (Direction, Position, Action) (Maybe Bullet)
bulletWire = execute_ $ return . update
  where update (direction, position, Attack) = Just Bullet { bulletPosition = position
                                                           , bulletVelocity = vector direction }
        update _                             = Nothing
        vector direction = (cos direction, sin direction) ^* bulletSpeed


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
  bullet'                           <- bulletWire                           -< (direction', position', action')

  returnA -< ( Just entity { entityState     = state'
                           , entityAge       = age'
                           , entityDirection = direction'
                           , entityPosition  = position'
                           , entityVelocity  = velocity'
                           , entityHealth    = health'
                           , entityEnergy    = energy' }
             , bullet' )

  where action0       = Action.Idle
        age0          = entityAge       entity
        direction0    = entityDirection entity
        energy0       = entityEnergy    entity
        health0       = entityHealth    entity
        position0     = entityPosition  entity
        velocity0     = entityVelocity  entity

-- Spawns a new entity.
spawnWire :: [Rectangle] -> EntityWire
spawnWire spawnRectangles = mkGen $ \dt (objects, action) -> do
  identifier <- Identifier.nextRandom
  spawnRectangle <- pick spawnRectangles
  let position = rectangleCentre spawnRectangle
  let wire = entityWire $ emptyEntity identifier position
  stepWire wire dt (objects, action)

module Entity where

import           Action
import           Bullet
import           Control.Wire hiding (object)
import qualified Control.Wire as Wire
import           Core
import           Data.Aeson
import           Data.Char (toLower)
import           Data.VectorSpace
import           Geometry
import           Identifier
import           Prelude hiding ((.), id)

-- The entity state.
data State =
    Idle
  | Attacking
  | Moving
  | Turning
  deriving (Eq, Show)

instance ToJSON State where
  toJSON s = toJSON $ map toLower $ show s

-- Represents an actor in the world.
data Entity = Entity
  {
    -- The entity identifier.
    entityId :: Identifier

    -- The age of the entity.
  , entityAge :: Age

    -- The position of the entity.
  , entityPosition :: Position

    -- The velcity of the entity.
  , entityVelocity :: Velocity

    -- The direction the entity is facting.
  , entityRotation :: Angle

    -- The health of the entity (when equal to zero the player is dead).
  , entityHealth :: Health

    -- The energy the entity has to perform actions.
  , entityEnergy :: Energy

    -- The entity state.
  , entityState :: State
  } deriving (Show)

instance ToJSON Entity where
  toJSON entity = object [ "id"       .= entityId       entity
                         , "age"      .= entityAge      entity
                         , "position" .= entityPosition entity
                         , "velocity" .= entityVelocity entity
                         , "rotation" .= entityRotation entity
                         , "health"   .= entityHealth   entity
                         , "energy"   .= entityEnergy   entity
                         , "state"    .= entityState    entity
                         ]

-- An entity wire takes an action and produces an entity and maybe a bullet.
type EntityWire = MyWire Action (Maybe Entity, Maybe Bullet)

entitySpeed :: Double
entitySpeed = 1

bulletSpeed :: Double
bulletSpeed = 1

-- Returns a new entity.
newEntity :: Identifier -> Position -> Entity
newEntity identifier position = Entity
  { entityId       = identifier
  , entityAge      = 0
  , entityPosition = position
  , entityVelocity = zeroV
  , entityRotation = 0
  , entityHealth   = 100
  , entityEnergy   = 100
  , entityState    = Entity.Idle
  }

-- If the entity has enough energy to perform the intended action then it
-- returns the new energy and the action. Otherwise it returns the original
-- energy value and forces the idle action.
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

-- The state wire outputs the current entity state. A 'Tick' message propagates
-- the previous entity state, any other message will change the entity state.
stateWire :: State -> MyWire Action State
stateWire = accum1 update
  where update state Tick = state
        update _ action = case action of
          Attack   -> Entity.Attacking
          Forward  -> Entity.Moving
          Reverse  -> Entity.Moving
          (Turn _) -> Entity.Turning
          _        -> Entity.Idle

-- The rotation wire outputs the current entity rotation. A 'Turn' message
-- rotates the entity to the given angle.
rotationWire :: Angle -> MyWire Action Angle
rotationWire = accum1 update
  where update _ (Turn rotation) = rotation
        update rotation _        = rotation

-- The velocity wire returns the current velocity for the entity. It changes
-- the velocity when it receives a forward/reverse action.
velocityWire :: Velocity -> MyWire (Angle, Action) Velocity
velocityWire = accum1 update
  where update _ (rotation, Forward) = vector rotation
        update _ (rotation, Reverse) = -(vector rotation)
        update velocity (_, Tick)    = velocity
        update _ _                   = zeroV
        vector rotation = (cos rotation, sin rotation) ^* entitySpeed

-- The health wire returns the current health of the entity. It inhibits when
-- the entity dies.
--
-- TODO: Health should depend on collisions with other entities/bullets.
healthWire :: Health -> MyWire Age Health
healthWire health0 = pure health0 . when (< 100000) <|> Wire.empty

-- The fire bullet wire spawns a new bullet if the player is attacking. The
-- bullet is fired from the entity's current position in the direction they are
-- facing.
fireBulletWire :: MyWire (Angle, Position, Action) (Maybe Bullet)
fireBulletWire = execute_ $ return . update
  where update (rotation, position, Attack) = Just Bullet { bulletPosition = position
                                                          , bulletVelocity = velocity rotation
                                                          }
        update _ = Nothing

        velocity rotation = (cos rotation, sin rotation) ^* bulletSpeed


-- Returns a new entity wire given an initial entity state.
entityWire :: Entity -> EntityWire
entityWire entity = proc action -> do
  age                            <- timeFrom age0                        -< ()
  (energy, action')              <- energyActionWire (energy0, action0)  -< action
  state                          <- stateWire state0                     -< action'
  rotation                       <- rotationWire rotation0               -< action'
  velocity                       <- velocityWire velocity0               -< (rotation, action')
  position                       <- integral1_ position0                 -< velocity
  health                         <- healthWire health0                   -< age
  bullet                         <- fireBulletWire                       -< (rotation, position, action')

  let entity' = Just entity { entityAge      = age
                            , entityPosition = position
                            , entityVelocity = velocity
                            , entityRotation = rotation
                            , entityHealth   = health
                            , entityEnergy   = energy
                            , entityState    = state
                            }

  returnA -< (entity', bullet)

  where action0   = Action.Idle
        age0      = entityAge      entity
        energy0   = entityEnergy   entity
        health0   = entityHealth   entity
        position0 = entityPosition entity
        rotation0 = entityRotation entity
        state0    = entityState    entity
        velocity0 = entityVelocity entity

-- Spawns a new entity.
spawnWire :: [Rectangle] -> EntityWire
spawnWire spawnRectangles = mkGen $ \dt action -> do
  identifier <- Identifier.nextRandom
  spawnRectangle <- pick spawnRectangles
  let position = rectangleCentre spawnRectangle
  let wire = entityWire $ newEntity identifier position
  stepWire wire dt action

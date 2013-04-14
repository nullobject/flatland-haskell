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
  { entityId       :: Identifier
  , entityState    :: State
  , entityAge      :: Age
  , entityPosition :: Position
  , entityVelocity :: Velocity
  , entityRotation :: Angle
  , entityHealth   :: Health
  , entityEnergy   :: Energy
  } deriving (Show)

instance ToJSON Entity where
  toJSON entity = object [ "id"       .= entityId       entity
                         , "state"    .= entityState    entity
                         , "age"      .= entityAge      entity
                         , "position" .= entityPosition entity
                         , "velocity" .= entityVelocity entity
                         , "rotation" .= entityRotation entity
                         , "health"   .= entityHealth   entity
                         , "energy"   .= entityEnergy   entity ]

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
  { entityId       = identifier
  , entityState    = Entity.Idle
  , entityAge      = 0
  , entityPosition = position
  , entityVelocity = zeroV
  , entityRotation = 0
  , entityHealth   = 100
  , entityEnergy   = 100 }

-- If the entity has enough energy to perform the action then it returns the
-- updated energy value and the action. Otherwise it returns the original
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

rotationWire :: Angle -> MyWire Action Angle
rotationWire = accum1 update
  where update rotation (Turn rotation') = rotation'
        update rotation _                = rotation

-- The velocity wire returns the current velocity for the entity. It changes
-- the velocity when it receives a forward/reverse action.
impulseWire :: MyWire (Angle, Action) Velocity
impulseWire = execute_ $ return . update
  where update (rotation, Forward) = vector rotation
        update (rotation, Reverse) = -(vector rotation)
        update _                   = zeroV
        vector rotation = (cos rotation, sin rotation) ^* entitySpeed

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

-- The fire bullet wire spawns a new bullet from the entity's current position.
fireBulletWire :: MyWire (Angle, Position, Action) (Maybe Bullet)
fireBulletWire = execute_ $ return . update
  where update (rotation, position, Attack) = Just Bullet { bulletPosition = position
                                                           , bulletVelocity = vector rotation }
        update _                             = Nothing
        vector rotation = (cos rotation, sin rotation) ^* bulletSpeed


-- Returns a new entity wire given an initial entity state.
entityWire :: Entity -> EntityWire
entityWire entity = proc (objects, action) -> do
  age'                              <- countFrom age0                       -< 1
  (energy', action')                <- energyActionWire (energy0, action0)  -< action
  state'                            <- stateWire                            -< action'
  rotation'                         <- rotationWire rotation0               -< action'
  impulse'                          <- impulseWire                          -< (rotation', action')
  (position', velocity', contacts') <- collisionWire (position0, velocity0) -< (objects, impulse')
  health'                           <- healthWire health0                   -< age'
  bullet'                           <- fireBulletWire                       -< (rotation', position', action')

  returnA -< ( Just entity { entityState    = state'
                           , entityAge      = age'
                           , entityPosition = position'
                           , entityVelocity = velocity'
                           , entityRotation = rotation'
                           , entityHealth   = health'
                           , entityEnergy   = energy' }
             , bullet' )

  where action0   = Action.Idle
        age0      = entityAge      entity
        energy0   = entityEnergy   entity
        health0   = entityHealth   entity
        position0 = entityPosition entity
        velocity0 = entityVelocity entity
        rotation0 = entityRotation entity

-- Spawns a new entity.
spawnWire :: [Rectangle] -> EntityWire
spawnWire spawnRectangles = mkGen $ \dt (objects, action) -> do
  identifier <- Identifier.nextRandom
  spawnRectangle <- pick spawnRectangles
  let position = rectangleCentre spawnRectangle
  let wire = entityWire $ emptyEntity identifier position
  stepWire wire dt (objects, action)

module Entity where

import           Action
import           Bullet
import           Collision
import           Control.Wire hiding (object)
import qualified Control.Wire as Wire
import           Core
import           Data.Aeson
import           Data.Char (toLower)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.VectorSpace
import           Geometry
import           Identifier
import           Prelude hiding ((.), id)
import           Wire

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
    -- The age of the entity.
    entityAge :: Age

    -- The moving body of the entity.
  , entityBody :: Body

    -- The health of the entity (when equal to zero the player is dead).
  , entityHealth :: Health

    -- The energy the entity has to perform actions.
  , entityEnergy :: Energy

    -- The entity state.
  , entityState :: State

    -- The bullet fired by the entity (if any).
  , entityBullet :: Maybe Bullet
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

-- An entity wire takes a message and produces an entity state.
type EntityWire = MyWire Message Entity

-- A map from identifiers to entities.
type EntityMap = Map Identifier Entity

-- An entity router is a wire which takes a list of messages and produces an
-- entities map.
type EntityRouter = MyWire [Message] EntityMap

-- A function which returns a new entity wire for the given identifier.
type EntityWireConstructor = Identifier -> EntityWire

entitySpeed :: Double
entitySpeed = 1

entityId :: Entity -> Identifier
entityId entity = bodyId $ entityBody entity

entityPosition :: Entity -> Position
entityPosition entity = bodyPosition $ entityBody entity

entityVelocity :: Entity -> Velocity
entityVelocity entity = bodyVelocity $ entityBody entity

entityRotation :: Entity -> Angle
entityRotation entity = bodyRotation $ entityBody entity

bulletSpeed :: Double
bulletSpeed = 1

-- Returns a new entity.
newEntity :: Identifier -> Position -> IO Entity
newEntity identifier position = return Entity { entityAge    = 0
                                              , entityBody   = body
                                              , entityHealth = 100
                                              , entityEnergy = 100
                                              , entityState  = Entity.Idle
                                              , entityBullet = Nothing
                                              }
  where body = (newBody identifier) {bodyPosition = position}

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

-- The state wire outputs the current entity state. A 'Update' message propagates
-- the previous entity state, any other message will change the entity state.
stateWire :: State -> MyWire Action State
stateWire = accum1 update
  where update state (Update _) = state
        update _ action = case action of
          Attack   -> Entity.Attacking
          Forward  -> Entity.Moving
          Reverse  -> Entity.Moving
          (Turn _) -> Entity.Turning
          _        -> Entity.Idle

-- The position wire outputs the current entity position.
positionWire :: Position -> MyWire Action Position
positionWire = accum1 update
  where update _ (Update body) = bodyPosition body
        update position _    = position

-- The velocity wire outputs the current entity velocity. It applies an impulse
-- when it receives a 'Forward' or 'Reverse' action.
velocityWire :: Velocity -> MyWire (Angle, Action) Velocity
velocityWire = accum1 update
  where update _ (rotation, Forward)   = vector rotation
        update _ (rotation, Reverse)   = -(vector rotation)
        update velocity (_, Update body) = bodyVelocity body
        update _ _                     = zeroV
        vector rotation = (cos rotation, sin rotation) ^* entitySpeed

-- The rotation wire outputs the current entity rotation. A 'Turn' message
-- rotates the entity to the given angle.
rotationWire :: Angle -> MyWire Action Angle
rotationWire = accum1 update
  where update _ (Turn rotation) = rotation
        update rotation _        = rotation

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
entityWire_ :: Entity -> EntityWire
entityWire_ entity = proc message -> do
  let action = messageAction message

  age                            <- timeFrom age0                        -< ()
  (energy, action')              <- energyActionWire (energy0, action0)  -< action
  state                          <- stateWire state0                     -< action'
  rotation                       <- rotationWire rotation0               -< action'
  velocity                       <- velocityWire velocity0               -< (rotation, action')
  position                       <- positionWire position0               -< action'
  health                         <- healthWire health0                   -< age
  bullet                         <- fireBulletWire                       -< (rotation, position, action')

  let body = body0 { bodyPosition = position
                   , bodyVelocity = velocity
                   , bodyRotation = rotation
                   }

  returnA -< entity { entityAge    = age
                    , entityBody   = body
                    , entityHealth = health
                    , entityEnergy = energy
                    , entityState  = state
                    , entityBullet = bullet
                    }

  where action0   = Action.Idle
        age0      = entityAge      entity
        body0     = entityBody     entity
        energy0   = entityEnergy   entity
        health0   = entityHealth   entity
        position0 = entityPosition entity
        rotation0 = entityRotation entity
        state0    = entityState    entity
        velocity0 = entityVelocity entity

-- Spawns a new entity with the given identifer, randomly at one of the spawn
-- points.
entityWire :: [Position] -> EntityWireConstructor
entityWire spawnPoints identifier = mkGen $ \dt message -> do
  -- Choose a random spawn point.
  position <- pick spawnPoints

  -- Create a new entity.
  entity <- newEntity identifier position

  -- Create an entity wire.
  let wire = entityWire_ entity

  stepWire wire dt message

-- Returns a wire which routes input messages to a map of entities.
entityRouter_
  :: EntityMap               -- ^ Map from identifiers to entities.
  -> (Message -> Identifier) -- ^ Function to turn the message into an identifier.
  -> EntityWireConstructor   -- ^ Base wire constructor.
  -> EntityRouter
entityRouter_ entitiesMap key constructor = entityRouter_' entitiesMap $ context_ key constructor
  where
    entityRouter_' :: EntityMap -> EntityWire -> EntityRouter
    entityRouter_' entitiesMap context = mkGen $ \dt messages -> do
      (entitiesMap', context') <- loop dt context entitiesMap messages
      return (Right entitiesMap', entityRouter_' entitiesMap' context')

    -- Steps the entity context with each message.
    loop :: Time -> EntityWire -> EntityMap -> [Message] -> IO (EntityMap, EntityWire)
    loop dt context entitiesMap [] = return (entitiesMap, context)
    loop dt context entitiesMap messages = do
      let message = head messages
      let messages' = tail messages
      (mx, context') <- stepWire context dt message
      let entitiesMap' = case mx of
                         Left _       -> Map.delete (key message) entitiesMap
                         Right entity -> Map.insert (key message) entity entitiesMap
      loop dt context' entitiesMap' messages'

-- Returns an entity router for the given entity constructor.
entityRouter :: EntityWireConstructor -> EntityRouter
entityRouter constructor = entityRouter_ Map.empty messageId constructor

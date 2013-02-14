{-# LANGUAGE DeriveGeneric, TupleSections #-}

module Entity where

import           Action
import           Control.Wire
import qualified Control.Wire as Wire
import           Core
import           Data.Aeson (toJSON, ToJSON)
import           Data.Char (toLower)
import           Data.VectorSpace
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
  , position  :: Vector
  , health    :: Health
  , energy    :: Energy
  } deriving (Generic, Show)

instance ToJSON Entity

-- An entity wire takes a message and produces a new entity state.
type EntityWire = MyWire (Maybe Action) (Maybe Entity)

-- Returns a new entity.
empty :: Identifier -> Entity
empty identifier = Entity
  { Entity.id = identifier
  , state     = Entity.Idle
  , age       = 0
  , direction = 0
  , position  = zeroV
  , health    = 100
  , energy    = 100
  }

directionWire :: Direction -> MyWire (Maybe Action) Direction
directionWire = accum1 f
  where f direction (Just (Turn direction')) = direction'
        f direction _                        = direction

positionWire :: Vector -> MyWire (Direction, Maybe Action) Vector
positionWire = accum1 f
  where f position (direction, Just Forward) = position ^+^ dir2vec direction
        f position (direction, Just Reverse) = position ^-^ dir2vec direction
        f position _                         = position

-- The health wire inhibits when the entity dies.
--
-- TODO: health should depend on collisions with other entities.
healthWire :: Health -> MyWire Age Health
healthWire health0 = pure health0 . when (< 100) <|> Wire.empty

energyWire :: Energy -> MyWire (Maybe Action) Energy
energyWire = accum1 f
  where f energy (Just Action.Idle) = energy + 10
        f energy _                  = energy - 10

stateWire :: MyWire (Maybe Action) State
stateWire = execute_ $ \action -> return $ case action of
  Just Attack   -> Entity.Attacking
  Just Forward  -> Entity.Moving
  Just Reverse  -> Entity.Moving
  Just (Turn _) -> Entity.Turning
  _             -> Entity.Idle

-- Returns a new entity wire given an initial entity state.
entityWire :: Entity -> EntityWire
entityWire entity = proc action -> do
  state'     <- stateWire                -< action
  age'       <- countFrom age0           -< 1
  direction' <- directionWire direction0 -< action
  position'  <- positionWire position0   -< (direction', action)
  health'    <- healthWire health0       -< age'
  energy'    <- energyWire energy0       -< action

  returnA -< Just entity { state     = state'
                         , age       = age'
                         , direction = direction'
                         , position  = position'
                         , health    = health'
                         , energy    = energy'
                         }

  where age0       = age entity
        direction0 = direction entity
        position0  = position entity
        health0    = health entity
        energy0    = energy entity

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
    Dead
  | Alive
  deriving (Eq, Generic, Show)

instance ToJSON State where
  toJSON s = toJSON $ map toLower $ show s

-- An entity is an actor in the world.
data Entity = Entity
  { id        :: Identifier
  , age       :: Age
  , direction :: Direction
  , position  :: Vector
  , health    :: Health
  , state     :: State
  } deriving (Generic, Show)

instance ToJSON Entity

-- An entity wire takes a message and produces a new entity state.
type EntityWire = MyWire (Maybe Action) (Maybe Entity)

-- Returns a new entity.
empty :: Identifier -> Entity
empty identifier = Entity
  { Entity.id = identifier
  , age       = 0
  , direction = 0
  , position  = zeroV
  , health    = 100
  , state     = Alive
  }

directionWire :: MyWire (Maybe Action) Direction
directionWire = accum1 f 0
  where
    f direction (Just (Turn direction')) = direction'
    f direction _ = direction

positionWire :: MyWire (Direction, Maybe Action) Vector
positionWire = accum1 f zeroV
  where
    f position (direction, Just Move) = position ^+^ dir2vec direction
    f position _ = position

stateWire :: MyWire Int State
stateWire = pure Alive . when (< 100) <|> Wire.empty

-- Returns a new entity wire given an initial entity state.
--
-- The entity wire inhibits when the entity dies.
entityWire :: Entity -> EntityWire
entityWire entity = proc action -> do
  age' <- countFrom 0 -< 1
  direction' <- directionWire -< action
  position' <- positionWire -< (direction', action)
  health' <- pure 100 -< ()
  state' <- stateWire -< age'
  returnA -< Just entity
    { age       = age'
    , direction = direction'
    , position  = position'
    , health    = health'
    , state     = state'
    }

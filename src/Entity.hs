{-# LANGUAGE DeriveGeneric, TupleSections #-}

module Entity where

import           Action
import           Control.Wire
import qualified Control.Wire as Wire
import           Core
import           Data.Aeson (toJSON, ToJSON)
import           Data.Char (toLower)
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
  { id       :: Identifier
  , age      :: Age
  , position :: Vector
  , velocity :: Vector
  , health   :: Health
  , state    :: State
  } deriving (Generic, Show)

instance ToJSON Entity

-- An entity wire takes a message and produces a new entity state.
type EntityWire = MyWire (Maybe Action) (Maybe Entity)

-- Returns a new entity.
empty :: Identifier -> Entity
empty identifier = Entity
  { Entity.id = identifier
  , age       = 0
  , position  = zeroVector
  , velocity  = zeroVector
  , health    = 100
  , state     = Alive
  }

stateWire :: MyWire Int State
stateWire = pure Alive . when (< 3) <|> Wire.empty

accelerationWire :: MyWire (Maybe Action) Vector
accelerationWire = execute_ action
  where action (Just (Move d)) = return (d, d)
        action _               = return (0, 0)

healthWire :: MyWire a Int
healthWire = pure 100

-- Returns a new entity wire given an initial entity state.
--
-- The entity wire inhibits when the entity dies.
entityWire :: Entity -> EntityWire
entityWire entity = proc action -> do
  age' <- countFrom 0 -< 1
  acceleration' <- accelerationWire -< action
  velocity' <- integral1_ zeroVector -< acceleration'
  position' <- integral1_ zeroVector -< velocity'
  health' <- healthWire -< ()
  state' <- stateWire -< age'
  returnA -< Just entity {
      age      = age'
    , position = position'
    , velocity = velocity'
    , health   = health'
    , state    = state'
    }

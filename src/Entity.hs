{-# LANGUAGE DeriveGeneric, TupleSections #-}

module Entity where

import Action
import Control.Wire
import Core
import Data.Aeson (toJSON, ToJSON)
import Data.Char (toLower)
import GHC.Generics (Generic)
import Identifier
import Prelude hiding ((.), id)

data State =
    Alive
  | Dead
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
type EntityWire = WireP (Maybe Action) Entity

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

stateWire :: WireP Int State
stateWire = pure Alive . when (< 3) <|> pure Dead

accelerationWire :: WireP a Vector
accelerationWire = pure (1, 1) . periodicallyI 1 <|> pure (0, 0)

healthWire :: WireP a Int
healthWire = pure 100

-- Returns a new entity wire given an initial entity state.
entityWire :: Entity -> EntityWire
entityWire entity = proc _ -> do
  age' <- countFrom 0 -< 1
  acceleration' <- accelerationWire -< ()
  velocity' <- integral1_ zeroVector -< acceleration'
  position' <- integral1_ zeroVector -< velocity'
  health' <- healthWire -< ()
  state' <- stateWire -< age'
  returnA -< entity {
      age      = age'
    , position = position'
    , velocity = velocity'
    , health   = health'
    , state    = state'
    }

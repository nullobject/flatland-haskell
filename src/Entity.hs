{-# LANGUAGE DeriveGeneric, TupleSections #-}

module Entity where

import           Control.Wire
import           Core (zeroVector, Action, Age, Health, Message, Vector)
import           Data.Aeson (toJSON, ToJSON)
import           Data.Char (toLower)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Traversable as Traversable
import qualified Data.Key as Key
import           GHC.Generics (Generic)
import           Identifier
import           Prelude hiding ((.), id)

data State = Alive | Dead deriving (Eq, Generic, Show)

instance ToJSON State where
  toJSON s = toJSON $ map toLower $ show s

-- An entity is an actor in the world.
data Entity = Entity {
  id       :: Identifier,
  age      :: Age,
  position :: Vector,
  velocity :: Vector,
  health   :: Health,
  state    :: State
} deriving (Generic, Show)

instance ToJSON Entity

-- An entity wire takes a message and produces a new entity state.
type EntityWire = WireP (Maybe Action) Entity

-- A map from an identifier to an entity wire.
type EntityWireMap = Map Identifier EntityWire

-- Returns a new entity.
empty :: Identifier -> Entity
empty identifier = Entity {
  Entity.id = identifier,
  age       = 0,
  position  = zeroVector,
  velocity  = zeroVector,
  health    = 100,
  state     = Alive
}

stateWire :: WireP Int State
stateWire = pure Alive . when (<5) <|> pure Dead

accelerationWire :: WireP a Vector
accelerationWire = pure (1, 1) . periodicallyI 1 <|> pure (0, 0)

-- Returns a new entity wire given an initial entity state.
entityWire :: Entity -> EntityWire
entityWire entity = proc _ -> do
  age' <- countFrom 0 -< 1
  acceleration' <- accelerationWire -< ()
  velocity' <- integral1_ zeroVector -< acceleration'
  position' <- integral1_ zeroVector -< velocity'
  state' <- stateWire -< age'
  returnA -< entity {age = age', position = position', velocity = velocity', state = state'}

-- Evolves a list of entity wires, routing actions which are addressed to them
-- by matching their identifiers. Actions which are addressed to unknown entity
-- wires are created using the constructor.
routeWire :: (Identifier -> EntityWire) -> WireP [Message] [Entity]
routeWire constructor = route Map.empty
  where
    route :: EntityWireMap -> WireP [Message] [Entity]
    route entityWireMap = mkGen $ \dt messages -> do
      -- Create a map from identifiers to actions.
      let actionMap = Map.fromList messages

      -- Ensure the messages can be delivered to entity wires.
      let entityWireMap' = foldl spawn entityWireMap $ Map.keys actionMap

      -- Step the entity wires, supplying the optional actions.
      res <- Key.mapWithKeyM (\identifier wire -> stepWire wire dt (Map.lookup identifier actionMap)) entityWireMap'

      -- WTF does this do?
      let resx = Traversable.sequence . fmap (\(mx, w) -> fmap (, w) mx) $ res

      return (fmap Map.elems (fmap (fmap fst) resx), route (fmap snd res))

    -- Spawns a new entity wire if one with the identifier doesn't already exist.
    spawn :: EntityWireMap -> Identifier -> EntityWireMap
    spawn entityWireMap identifier = Map.alter f identifier entityWireMap
      where
        f = Just . maybe wire Control.Wire.id
        wire = constructor identifier

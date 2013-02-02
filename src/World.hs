module World where

import           Control.Wire
import           Core
import           Entity (Entity)
import qualified Entity
import           Identifier
import           Prelude hiding ((.), id)

-- A world contains a list of entities.
data World = World {
  age      :: Age,
  entities :: [Entity]
} deriving (Show)

-- A world wire takes a list of messages and produces a new world state.
type WorldWire = WireP [Message] World

-- Returns a new world.
empty :: World
empty = World {
  age      = 0,
  entities = []
}

-- Returns a new world wire given an initial world state.
worldWire :: World -> WorldWire
worldWire world = proc messages -> do
  age' <- countFrom 0 -< 1
  entities' <- Entity.routeWire $ Entity.entityWire . Entity.empty -< messages
  returnA -< world {age = age', entities = entities'}

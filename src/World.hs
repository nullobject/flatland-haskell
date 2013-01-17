module World where

import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.UUID (UUID)
import           Entity (Entity)
import qualified Entity

data World = World {
  entities :: Map UUID Entity,
  age      :: Int
} deriving (Show)

type WorldState = StateT World IO

-- Returns a new world.
empty :: World
empty = World {
  entities = Map.empty,
  age      = 0
}

-- Increments the age of the world.
incrementAge :: World -> World
incrementAge world = world {age = age'}
  where age' = age world + 1

-- Ticks the entities in the world.
tickEntities :: World -> World
tickEntities world = world {entities = entities'}
  where entities' = Map.map Entity.tick $ entities world

-- Adds an entity to the world.
addEntity :: Entity -> World -> World
addEntity entity world = world {entities = entities'}
  where
    uuid = Entity.id entity
    entities' = Map.insert uuid entity $ entities world

-- Ticks the world.
tick :: WorldState ()
tick = modify $ incrementAge . tickEntities

-- Spawns an entity in the world.
spawn :: Entity -> WorldState ()
spawn entity = modify $ addEntity entity

-- Moves an entity in the world.
move :: UUID -> WorldState ()
move uuid = do
  world <- get
  let entities' = Map.adjust Entity.move uuid $ entities world
  put world {entities = entities'}

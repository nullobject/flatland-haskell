module World where

import           Control.Wire
import           Core
import           Entity (Entity)
import           Player (Player)
import qualified Player
import           Identifier
import           Prelude hiding ((.), id)

-- A world contains a list of players.
data World = World
  { age      :: Age
  , players  :: [Player]
  } deriving (Show)

-- A world wire takes a list of messages and produces a new world state.
type WorldWire = WireP [Message] World

-- Returns a new world.
empty :: World
empty = World
  { age     = 0
  , players = []
  }

-- TODO: map the entities from the player wires.
entities :: World -> [Entity]
entities _ = []

-- Returns a new world wire given an initial world state.
worldWire :: World -> WorldWire
worldWire world = proc messages -> do
  age' <- countFrom 0 -< 1
  players' <- Player.routeWire $ Player.playerWire . Player.empty -< messages
  returnA -< world {age = age', players = players'}

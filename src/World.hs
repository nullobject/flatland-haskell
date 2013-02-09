module World where

import           Control.Wire
import           Core
import qualified Data.Maybe as Maybe
import           Entity (Entity)
import           Identifier
import           Player (Player)
import qualified Player
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
entities world = Maybe.catMaybes $ map Player.entity $ players world

-- Returns a new world wire given an initial world state.
worldWire :: World -> WorldWire
worldWire world = proc messages -> do
  age' <- countFrom 0 -< 1
  players' <- Player.routeWire $ Player.playerWire . Player.empty -< messages
  returnA -< world {age = age', players = players'}

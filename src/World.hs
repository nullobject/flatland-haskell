{-# LANGUAGE DeriveGeneric #-}

module World where

import           Control.Wire
import           Core
import           Data.Aeson (ToJSON)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import           Entity (Entity)
import           Geometry (Segment (..))
import           GHC.Generics (Generic)
import           Identifier
import           Player (Player)
import qualified Player
import           Prelude hiding ((.), id)

-- A world contains a list of players.
data World = World
  { age      :: Age
  , players  :: [Player]
  , segments :: [Segment]
  } deriving (Generic, Show)

instance ToJSON World

-- A world wire takes a list of messages and produces a new world state.
type WorldWire = MyWire [Message] World

-- Returns a new world.
empty :: World
empty =
  World { age      = 0
        , players  = []
        , segments = segments
        }
  where segments = [ Segment (-1,  1) ( 1,  1)
                   , Segment ( 1,  1) ( 1, -1)
                   , Segment ( 1, -1) (-1, -1)
                   , Segment (-1, -1) (-1,  1)
                   , Segment (-5,  5) ( 5,  5)
                   , Segment ( 5,  5) ( 5, -5)
                   , Segment ( 5, -5) (-5, -5)
                   , Segment (-5, -5) (-5,  5) ]

-- Returns the player with the given identifier.
getPlayer :: Identifier -> World -> Maybe Player
getPlayer identifier world = List.find predicate $ players world
  where predicate = \player -> Player.id player == identifier

-- Returns the entities in the world.
entities :: World -> [Entity]
entities world = Maybe.catMaybes $ map Player.entity $ players world

-- Returns a new world wire given an initial world state.
worldWire :: World -> WorldWire
worldWire world = proc messages -> do
  age' <- countFrom 0 -< 1
  players' <- Player.routeWire $ Player.playerWire . Player.empty -< messages
  returnA -< world {age = age', players = players'}

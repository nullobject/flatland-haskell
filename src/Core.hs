module Core where

import Action
import Control.Wire
import Identifier

type Age     = Int
type Health  = Int
type Energy  = Int
type Message = (Identifier, Action)
type Vector  = (Double, Double)
type MyWire  = Wire LastException IO

readMaybe :: (Read a) => String -> Maybe a
readMaybe s =
  case [x | (x, t) <- reads s, ("", "") <- lex t] of
    [x] -> Just x
    _   -> Nothing

-- Another wire is constructed whenever the given wire wire inhibits.
continually :: MyWire a b -> MyWire a b
continually wire = switchBy (const wire) wire

-- Converts the given direction (in radians) to a unit vector.
dir2vec :: Direction -> Vector
dir2vec d = (cos d, sin d)

-- Returns the cost of action.
cost :: Action -> Energy
cost action = case action of
  Idle    ->  10
  Attack  -> -30
  Forward -> -20
  Reverse -> -20
  Turn _  -> -20
  _       ->   0

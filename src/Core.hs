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

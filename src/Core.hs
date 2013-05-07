module Core where

import Action
import Control.Wire
import Identifier
import Prelude hiding ((.), id)

type Age     = Double
type Health  = Int
type Energy  = Int
type Message = (Identifier, Action)
type MyWire  = Wire LastException IO

-- Returns the message identifier.
messageId :: Message -> Identifier
messageId = fst

-- Returns the message action.
messageAction :: Message -> Action
messageAction = snd

-- Reads a possible value from string.
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case [x | (x, t) <- reads s, ("", "") <- lex t] of
  [x] -> Just x
  _   -> Nothing

-- Constructs another wire whenever the given wire inhibits.
continually :: MyWire a b -> MyWire a b
continually wire = switchBy (const wire) wire

-- Rotates the elements of the list to the right.
rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

-- Picks a random element from the list.
pick :: [a] -> IO a
pick xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)

module Core where

import           Action
import           Control.Wire
import           Data.Map (Map)
import qualified Data.Map as Map
import           Identifier
import           Prelude hiding ((.), id)

type Age     = Double
type Health  = Int
type Energy  = Int
type Message = (Identifier, Action)
type MyWire  = Wire LastException IO

-- A map from identifiers to messages.
type MessageMap = Map Identifier Message

-- Returns the message identifier.
messageId :: Message -> Identifier
messageId = fst

-- Returns the message action.
messageAction :: Message -> Action
messageAction = snd

-- Returns a message map for the given messages.
mapMessages :: [Message] -> MessageMap
mapMessages messages = foldl f Map.empty messages
  where f messageMap message = Map.insert (messageId message) message messageMap

-- Reads a possible value from string.
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case [x | (x, t) <- reads s, ("", "") <- lex t] of
  [x] -> Just x
  _   -> Nothing

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _         = error "Either.fromRight: Nothing"

isRight :: Either a b -> Bool
isRight (Right b) = True
isRight _         = False

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

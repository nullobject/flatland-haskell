module Core where

import Identifier

type Age = Int

type Health = Int

data Action = Idle | Attack | Move Double | Turn Int deriving (Show)

type Message = (Identifier, Action)

type Vector = (Double, Double)

zeroVector :: Vector
zeroVector = (0, 0)

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case [x | (x, t) <- reads s, ("", "") <- lex t] of
                [x] -> Just x
                _   -> Nothing

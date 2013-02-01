module Core where

import Data.UUID (UUID)

type Identifier = UUID

data Action = Idle | Attack | Move | Turn Int deriving (Show)

type Vector = (Double, Double)

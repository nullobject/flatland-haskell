{-# LANGUAGE DeriveGeneric #-}

module Entity where

import Core (Identifier, Vector)
import Data.Aeson (toJSON, ToJSON)
import Data.Char (toLower)
import Data.UUID (UUID)
import GHC.Generics (Generic)

type Age    = Int
type Health = Int

data StateName = Dead | Idle deriving (Eq, Generic, Show)

instance ToJSON StateName where
  toJSON s = toJSON $ map toLower $ show s

instance ToJSON UUID where
  toJSON uuid = toJSON $ show uuid

data Entity = Entity {
  id       :: Identifier,
  state    :: StateName,
  health   :: Health,
  position :: Vector,
  age      :: Age
} deriving (Generic, Show)

instance ToJSON Entity

-- Returns a new entity.
empty :: Identifier -> Entity
empty identifier = Entity {
  Entity.id = identifier,
  state     = Idle,
  health    = 100,
  position  = (0, 0),
  age       = 0
}

-- Increments the age of the entity.
incrementAge :: Entity -> Entity
incrementAge entity = entity {age = age'}
  where age' = age entity + 1

-- Ticks the entity.
tick :: Entity -> Entity
tick entity = incrementAge entity

-- Moves the entity.
move :: Entity -> Entity
move entity = entity {position = (x + 1, y + 1)}
  where (x, y) = position entity

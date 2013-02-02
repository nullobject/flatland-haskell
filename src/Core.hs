module Core where

import           Data.Aeson (toJSON, ToJSON)
import           Data.Char (isSpace)
import           Data.UUID (UUID)
import qualified Data.UUID as UUID

newtype Identifier = Identifier UUID deriving (Eq, Ord)

instance Read Identifier where
  readsPrec _ str =
    let noSpaces = dropWhile isSpace str
    in case UUID.fromString (take 36 noSpaces) of
      Nothing -> []
      Just u  -> [(Identifier u, drop 36 noSpaces)]

instance Show Identifier where
  show (Identifier uuid) = show uuid

instance ToJSON Identifier where
  toJSON identifier = toJSON $ show identifier

type Age = Int

type Health = Int

data Action = Idle | Attack | Move Double | Turn Int deriving (Show)

type Vector = (Double, Double)

zeroVector :: Vector
zeroVector = (0, 0)

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case [x | (x, t) <- reads s, ("", "") <- lex t] of
                [x] -> Just x
                _   -> Nothing

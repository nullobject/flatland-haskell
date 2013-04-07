module Collision
  ( calculateCollisions
  , collideWithObjects
  , intersectAABB
  , getRectangleAABB
  , AABB (..)
  , Contact (..)
  ) where

import           Data.Aeson (toJSON, ToJSON)
import qualified Data.Maybe as Maybe
import           Data.VectorSpace
import           Geometry (Extents, Point, Position, Rectangle (..), Velocity)
import qualified Geometry

-- An axis-aligned bounding box.
data AABB = AABB Point Extents deriving (Eq, Show)

-- A collision contact.
data Contact = Contact Double Double deriving (Eq, Show)

-- Converts the given rectangle to an AABB.
getRectangleAABB :: Rectangle -> AABB
getRectangleAABB (Rectangle position extents) = AABB centre halfExtents
  where centre = position ^+^ halfExtents
        halfExtents = extents ^/ 2

-- Calculates the collision between the two moving AABBs using the
-- separating-axis test.
calculateCollisions :: AABB -> AABB -> Velocity -> Velocity -> Maybe Contact
calculateCollisions a b aVelocity bVelocity
  -- Initially overlapping.
  | intersectAABB a b = Just (Contact 0 0)

  -- Non-intersecting and moving apart (or stationary) on an axis.
  | bMax0 < aMin0 && v0 <= 0 = Nothing
  | bMin0 > aMax0 && v0 >= 0 = Nothing
  | bMax1 < aMin1 && v1 <= 0 = Nothing
  | bMin1 > aMax1 && v1 >= 0 = Nothing

  -- Intersecting.
  | tFirst <= 1 && tFirst <= tLast = Just (Contact tFirst tLast)

  | otherwise = Nothing

  where tFirst = max tFirst0 tFirst1
        tLast  = min tLast0 tLast1

        -- X-axis first/last contact times.
        (tFirst0, tLast0) = axisContactTimes (aMin0, aMax0) (bMin0, bMax0) v0

        -- Y-axis first/last contact times.
        (tFirst1, tLast1) = axisContactTimes (aMin1, aMax1) (bMin1, bMax1) v1

        ((aMin0, aMin1), (aMax0, aMax1)) = calculateAABBExtents a
        ((bMin0, bMin1), (bMax0, bMax1)) = calculateAABBExtents b

        (v0, v1) = bVelocity ^-^ aVelocity

axisContactTimes :: Extents -> Extents -> Double -> Extents
axisContactTimes (aMin, aMax) (bMin, bMax) v = (tFirst, tLast)
  where tFirst
          | aMax < bMin && v < 0 = (aMax - bMin) / v
          | bMax < aMin && v > 0 = (aMin - bMax) / v
          | otherwise = 0

        tLast
          | bMax > aMin && v < 0 = (aMin - bMax) / v
          | aMax > bMin && v > 0 = (aMax - bMin) / v
          | otherwise = 1

calculateAABBExtents :: AABB -> (Extents, Extents)
calculateAABBExtents (AABB centre extents) = (centre ^-^ extents, centre ^+^ extents)

-- Returns true if the given AABBs are intersecting, false otherwise.
intersectAABB :: AABB -> AABB -> Bool
intersectAABB (AABB (aCentre0, aCentre1) (aRadius0, aRadius1)) (AABB (bCentre0, bCentre1) (bRadius0, bRadius1))
  | abs (aCentre0 - bCentre0) > (aRadius0 + bRadius0) = False
  | abs (aCentre1 - bCentre1) > (aRadius1 + bRadius1) = False
  | otherwise = True

-- Collides the object with the given position and velocity with a list of
-- objects and returns the updated position, velocity and a list of contacts.
collideWithObjects :: [AABB] -> Position -> Velocity -> (Position, Velocity, [Contact])
collideWithObjects objects position velocity = (position', velocity', contacts')
  where
    -- Collide with each object.
    (velocity', contacts') = foldl (collide position) (velocity, []) objects

    -- Integrate the position.
    position' = position ^+^ velocity'

-- Collides with the given object and resolves any collisions.
collide :: Position -> (Velocity, [Contact]) -> AABB -> (Velocity, [Contact])
collide position (velocity, contacts) that = (velocity', contacts')
  where this      = AABB position (0.5, 0.5)
        contact   = calculateCollisions this that velocity zeroV
        velocity' = applyContact velocity contact
        contacts' = contacts ++ Maybe.maybeToList contact

-- Corrects the velocity for the given contact so that when the position is
-- integrated the objects will only just be touching.
applyContact :: Velocity -> Maybe Contact -> Velocity
applyContact velocity Nothing = velocity
applyContact velocity (Just (Contact tFirst _)) = velocity'
  where velocity' = lerp zeroV velocity (tFirst - epsilon)
        epsilon = 0.00000001

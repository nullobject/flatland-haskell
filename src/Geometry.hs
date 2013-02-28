{-# LANGUAGE DeriveGeneric #-}

module Geometry where

import Core
import Data.Aeson (ToJSON)
import Data.AffineSpace
import Data.VectorSpace
import GHC.Generics (Generic)

type Angle = Double
type Point = Vector

-- A line segment between two points.
data Segment = Segment Point Point deriving (Generic, Eq, Show)

instance ToJSON Segment

data Triangle = Triangle Point Point Point deriving (Eq, Show)

-- Calculates the angle between two points.
angleBetween :: Point -> Point -> Angle
angleBetween (p1, p2) (q1, q2) = atan2 (q2 - p2) (q1 - p1)

-- Calculates the difference between two angles.
angleDifference :: Angle -> Angle -> Angle
angleDifference a b
  | d < -pi   = d + 2 * pi
  | d >  pi   = d - 2 * pi
  | otherwise = d
  where d = b - a

-- Calculates the distance from a point to a line segment.
--
-- See:
--   http://paulbourke.net/geometry/pointlineplane/
--   http://mathworld.wolfram.com/Point-LineDistance2-Dimensional.html
--   http://stackoverflow.com/a/1501725/1284705
pointSegmentDistance :: Point -> Segment -> Double
pointSegmentDistance p (Segment v w)
  | t < 0     = distance p v
  | t > 1     = distance p w
  | otherwise = distance p r
  where t = ((p - v) <.> (w - v)) / magnitudeSq (w - v)
        r = v + (t *^ (w - v))

-- See:
--   http://paulbourke.net/geometry/pointlineplane/
--   http://mathworld.wolfram.com/Line-LineIntersection.html
--   http://stackoverflow.com/a/565282/1284705
intersectLines :: Point -> Point -> Point -> Point -> Point
intersectLines p r q s = p + ((r - p) ^* t)
  where t = (q - p) <*> (s - q) / (r - p) <*> (s - q)

-- 2D vector cross product.
(<*>) :: Point -> Point -> Double
(<*>) (p1, p2) (q1, q2) = p1 * q2 - p2 * q1

-- Returns true if the point is in the triangle.
--
-- See:
--   http://www.blackpawn.com/texts/pointinpoly/
intersects :: Point -> Triangle -> Bool
intersects p (Triangle a b c) = (u >= 0) && (v >= 0) && (u + v <= 1)
  where v0 = c - a
        v1 = b - a
        v2 = p - a

        dot00 = v0 <.> v0
        dot01 = v0 <.> v1
        dot02 = v0 <.> v2
        dot11 = v1 <.> v1
        dot12 = v1 <.> v2

        invDenom = 1 / (dot00 * dot11 - dot01 * dot01)

        u = (dot11 * dot02 - dot01 * dot12) * invDenom
        v = (dot00 * dot12 - dot01 * dot02) * invDenom

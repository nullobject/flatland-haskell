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

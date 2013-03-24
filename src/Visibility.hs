module Visibility
  ( calculateEndpoints
  , calculateVisibility
  , Endpoint (..)
  ) where

import Core (rotate)
import Data.List (delete, insertBy, sort)
import Data.VectorSpace
import Geometry

-- An endpoint to be processed by the visibility algorithm.
data Endpoint = Endpoint Point Segment Angle Bool deriving (Eq, Show)

instance Ord Endpoint where
  -- Compares two endpoints by angle and whether they begin a segment.
  compare (Endpoint _ _ angle1 begin1) (Endpoint _ _ angle2 begin2)
    | angles /= EQ         = angles
    | begin1 && not begin2 = LT
    | begin2 && not begin1 = GT
    | otherwise            = EQ
    where angles = angle1 `compare` angle2

-- Returns a list of triangles representing the area visible from the origin.
--
-- The algorithm casts rays from the origin to the vertices in each polygon,
-- calculating the triangles subtended by the rays intersecting with the
-- nearest edge.
calculateVisibility :: Point -> [Polygon] -> [Triangle]
calculateVisibility origin polygons = calculateTriangles origin endpoints
  where endpoints = concatMap (calculateEndpoints origin) segments
        segments = concatMap calculateSegments polygons

-- Calculates the endpoints for the segment.
calculateEndpoints :: Point -> Segment -> [Endpoint]
calculateEndpoints origin segment@(Segment a b) =
  [ Endpoint a segment angle1 begin1
  , Endpoint b segment angle2 begin2 ]
  where angle1 = angleBetween origin a
        angle2 = angleBetween origin b
        begin1 = angleDifference angle1 angle2 > 0
        begin2 = not begin1

-- Calculates a list of triangles from a list of endpoints. The algorithm makes
-- two passes over the list of endpoints and snaps each of the resulting
-- triangles.
calculateTriangles :: Point -> [Endpoint] -> [Triangle]
calculateTriangles origin endpoints = map snapTriangle triangles
  where (angle, segments, _) = foldl step' (0, [], []) endpoints'
        (_, _, triangles)    = foldl step' (angle, segments, []) endpoints'
        endpoints' = sort endpoints
        step' = step origin

-- Steps the algorithm to the next endpoint, producing a new triangle if the
-- closest segment changes.
step :: Point -> (Angle, [Segment], [Triangle]) -> Endpoint -> (Angle, [Segment], [Triangle])
step origin (angle, openSegments, triangles) (Endpoint _ segment currentAngle begin) = (angle', openSegments', triangles')
  where openSegments' = if begin
                        then insertBy (comparator origin) segment openSegments
                        else delete segment openSegments

        openSegment = if null openSegments
                      then Nothing
                      else Just $ head openSegments

        openSegment' = if null openSegments'
                       then Nothing
                       else Just $ head openSegments'

        angle' = if openSegment /= openSegment'
                 then currentAngle
                 else angle

        triangles' = if angle /= angle'
                     then triangle:triangles
                     else triangles

        triangle = calculateTriangle angle angle' origin openSegment

-- Compares the distance of the two segments relative to the origin.
comparator :: Point -> Segment -> Segment -> Ordering
comparator origin p q = d1 `compare` d2
  where d1 = pointSegmentDistance origin p
        d2 = pointSegmentDistance origin q

-- Creates a new triangle by intersecting the rays subtended by the two angles
-- with the segment.
calculateTriangle :: Angle -> Angle -> Point -> Maybe Segment -> Triangle

calculateTriangle angle1 angle2 a (Just (Segment b c))
  | t == 0    = Triangle a c b -- The lines don't intersect.
  | otherwise = Triangle a p q -- The lines intersect.
  where d = a + (cos angle1, sin angle1)
        e = a + (cos angle2, sin angle2)
        p = intersectLines b c a d
        q = intersectLines b c a e
        t = (c - b) <*> (d - a)

calculateTriangle angle1 angle2 a Nothing = Triangle a p q
  where b = a + (cos angle1, sin angle1) ^* 500
        c = a + (cos angle2, sin angle2) ^* 500
        d = a + (cos angle1, sin angle1)
        e = a + (cos angle2, sin angle2)
        p = intersectLines b c a d
        q = intersectLines b c a e

snapTriangle :: Triangle -> Triangle
snapTriangle (Triangle a b c) = Triangle (snapPoint a) (snapPoint b) (snapPoint c)

snapPoint :: Point -> Point
snapPoint (p1, p2) = (round' p1 8, round' p2 8)
  where round' f n = (fromInteger $ round $ f * (10 ^ n)) / (10.0 ^^ n)

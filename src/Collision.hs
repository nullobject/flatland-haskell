module Collision
  ( calculateCollision
  , intersectAABB
  , getRectangleAABB
  , runPhysics
  , AABB (..)
  , Body (..)
  , Collision (..)
  ) where

import Data.List
import Data.Maybe
import Data.VectorSpace
import Geometry

type Time = Double

-- Represents an axis-aligned bounding box.
data AABB = AABB Point Extents deriving (Eq, Show)

-- Represents a rigid body.
data Body = Body
  {
    -- The position of the body.
    bodyPosition :: Position

    -- The extents of the body.
  , bodyExtents :: Extents

    -- The velocity of the body.
  , bodyVelocity :: Velocity

    -- The direction the body is facing.
  , bodyRotation :: Angle

    -- The inverse mass of the body.
  , bodyInverseMass :: Double
  } deriving (Eq, Show)

-- Represents a collision between two bodies.
data Collision = Collision
  {
    -- The bodies in the collision.
    collisionBodies :: (Body, Body)

    -- The time of first contact.
  , collisionFirstContactTime :: Time

    -- The time of last contact.
  , collisionLastContactTime :: Time

    -- The contact normal.
  , collisionNormal :: Vector
  } deriving (Eq, Show)

instance Ord Collision where
  a <= b = a0 <= b0 && a1 <= b1
    where a0 = collisionFirstContactTime a
          a1 = collisionLastContactTime a
          b0 = collisionFirstContactTime b
          b1 = collisionLastContactTime b

-- Returns a list of 2-combinations without repetition.
pairs xs = [(a, b) | (a:as) <- init . tails $ xs, b <- as]

-- Returns a list of 2-combinations with repetition.
pairs_ xs = [(a, b) | as@(a:_) <- init . tails $ xs, b <- as]

-- Returns the bodies in the collision as a list.
collisionBodies_ :: Collision -> [Body]
collisionBodies_ collision = [bodyA, bodyB]
  where (bodyA, bodyB) = collisionBodies collision

-- Runs the physics simulation for the bodies over the given time interval.
runPhysics :: [Body] -> Time -> [Body]
runPhysics bodies 0 = bodies
runPhysics bodies dt
  -- If there were no collisions, then we're done.
  | null collisions = integrateBodies bodies dt

  -- If the first collision occurs after the time interval, then we're done.
  | tFirst > dt = integrateBodies bodies dt

  -- Otherwise, integrate the bodies until the time of the first collision.
  -- Resolve the collision, then run the simulation for the remainder of the
  -- time interval.
  | otherwise = runPhysics bodies' dt'

  where collisions = calculateCollisions bodies dt
        firstCollision = minimum collisions
        tFirst = collisionFirstContactTime firstCollision
        tFirst' = tFirst - epsilon
        dt' = dt - tFirst'
        epsilon = 1.0e-8
        nonCollidingBodies = bodies \\ collisionBodies_ firstCollision
        bodies' = integrateBodies nonCollidingBodies tFirst' ++ resolveCollision firstCollision tFirst'

-- Resolves the velocities of the bodies in the given collision.
--
-- Refer to chapter 7 of Game Physics Engine Development.
resolveCollision :: Collision -> Time -> [Body]
resolveCollision collision dt
  -- If the bodies are already separating, then there's no impulse required.
  | separatingVelocity > 0 = [bodyA', bodyB']

  -- If the bodies have infinite mass, then impulses have no effect.
  | totalInverseMass <= 0 = [bodyA', bodyB']

  -- Otherwise, apply an impulse proportional to the inverse mass of the bodies.
  | otherwise = [
                  bodyA' {bodyVelocity = velocityA ^+^ inverseMassA *^ impulsePerUnitInverseMass}
                , bodyB' {bodyVelocity = velocityB ^-^ inverseMassB *^ impulsePerUnitInverseMass}
                ]

  where (bodyA, bodyB) = collisionBodies collision
        bodyA' = integrateBody bodyA dt
        bodyB' = integrateBody bodyB dt
        separatingVelocity = (velocityA ^-^ velocityB) <.> collisionNormal collision
        separatingVelocity' = -restitution * separatingVelocity
        deltaVelocity = separatingVelocity' - separatingVelocity
        totalInverseMass = inverseMassA + inverseMassB
        impulse = deltaVelocity / totalInverseMass
        impulsePerUnitInverseMass = collisionNormal collision ^* impulse
        velocityA = bodyVelocity bodyA
        velocityB = bodyVelocity bodyB
        inverseMassA = bodyInverseMass bodyA
        inverseMassB = bodyInverseMass bodyB
        restitution = 1.0

-- Integrates the bodies over the given time interval.
integrateBodies :: [Body] -> Time -> [Body]
integrateBodies bodies dt = map (\body -> integrateBody body dt) bodies

-- Integrates the body over the given time interval.
integrateBody :: Body -> Time -> Body
integrateBody body 0 = body
integrateBody body dt = body {bodyPosition = position'}
  where position  = bodyPosition body
        velocity  = bodyVelocity body
        position' = position ^+^ (velocity ^* dt)

-- Returns the collisions between the bodies over the given time interval.
calculateCollisions :: [Body] -> Time -> [Collision]
calculateCollisions bodies dt = mapMaybe (uncurry collideBodies) $ pairs bodies

-- Returns a possible collision between the given bodies.
collideBodies :: Body -> Body -> Maybe Collision
collideBodies bodyA bodyB
  | isJust contact = Just collision
  | otherwise      = Nothing
  where contact = calculateCollision (AABB positionA extentsA) (AABB positionB extentsB) velocityA velocityB
        Just (tFirst, tLast) = contact
        collision = Collision (bodyA, bodyB) tFirst tLast normal
        normal = normalized $ positionA ^-^ positionB
        velocityA = bodyVelocity bodyA
        velocityB = bodyVelocity bodyB
        positionA = bodyPosition bodyA
        positionB = bodyPosition bodyB
        extentsA = bodyExtents bodyA
        extentsB = bodyExtents bodyB

-- Calculates the time of first and last contact between the given bodies using
-- the separating-axis test.
calculateCollision :: AABB -> AABB -> Velocity -> Velocity -> Maybe (Time, Time)
calculateCollision a b velocityA velocityB
  -- Initially overlapping.
  | intersectAABB a b = Just (0, 0)

  -- Non-intersecting and moving apart (or stationary) on an axis.
  | bMax0 < aMin0 && v0 <= 0 = Nothing
  | bMin0 > aMax0 && v0 >= 0 = Nothing
  | bMax1 < aMin1 && v1 <= 0 = Nothing
  | bMin1 > aMax1 && v1 >= 0 = Nothing

  -- Intersecting.
  | tFirst <= 1 && tFirst <= tLast = Just (tFirst, tLast)

  | otherwise = Nothing

  where tFirst = max tFirst0 tFirst1
        tLast  = min tLast0 tLast1

        -- X-axis first/last contact times.
        (tFirst0, tLast0) = axisCollisionTimes (aMin0, aMax0) (bMin0, bMax0) v0

        -- Y-axis first/last contact times.
        (tFirst1, tLast1) = axisCollisionTimes (aMin1, aMax1) (bMin1, bMax1) v1

        ((aMin0, aMin1), (aMax0, aMax1)) = calculateAABBExtents a
        ((bMin0, bMin1), (bMax0, bMax1)) = calculateAABBExtents b

        (v0, v1) = velocityB ^-^ velocityA

axisCollisionTimes :: Extents -> Extents -> Time -> Extents
axisCollisionTimes (aMin, aMax) (bMin, bMax) v = (tFirst, tLast)
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

-- Converts the given rectangle to an AABB.
getRectangleAABB :: Rectangle -> AABB
getRectangleAABB (Rectangle position extents) = AABB centre halfExtents
  where centre = position ^+^ halfExtents
        halfExtents = extents ^/ 2

{-# LANGUAGE DeriveGeneric #-}

module Collision
  ( calculateCollision
  , getRectangleAABB
  , intersectAABB
  , newBody
  , runPhysics
  , AABB (..)
  , Body (..)
  , Collision (..)
  ) where

import Data.Aeson
import Data.List
import Data.Maybe
import Data.VectorSpace
import Geometry
import GHC.Generics (Generic)
import Identifier

type Time = Double

-- Represents an axis-aligned bounding box.
data AABB = AABB
  {
    -- The centre of the AABB.
    aabbCentre :: Point

    -- The extents (half-width & half-height from the centre) of the AABB.
  , aabbExtents :: Extents
  } deriving (Eq, Show)

-- Represents a rigid body.
data Body = Body
  {
    -- The unique identifier of the body.
    bodyId :: Identifier

    -- The position of the body.
  , bodyPosition :: Position

    -- The velocity of the body.
  , bodyVelocity :: Velocity

    -- The direction the body is facing.
  , bodyRotation :: Angle

    -- The inverse mass of the body.
  , bodyInverseMass :: Double

    -- The extents of the body.
  , bodyExtents :: Extents
  } deriving (Eq, Generic, Show)

instance FromJSON Body
instance ToJSON Body

-- Represents a collision between two bodies.
data Collision = Collision
  {
    -- The bodies involved in the collision.
    collisionBodies :: [Body]

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
pairs :: [a] -> [(a, a)]
pairs xs = [(a, b) | (a:as) <- init . tails $ xs, b <- as]

-- Returns a list of 2-combinations with repetition.
pairs_ :: [a] -> [(a, a)]
pairs_ xs = [(a, b) | as@(a:_) <- init . tails $ xs, b <- as]

newBody :: Identifier -> Body
newBody identifier = Body { bodyId          = identifier
                          , bodyPosition    = zeroV
                          , bodyVelocity    = zeroV
                          , bodyRotation    = 0
                          , bodyExtents     = (0.5, 0.5)
                          , bodyInverseMass = 1
                          }

-- Runs the physics simulation for the given static geometry and dynamic bodies
-- over a time interval.
runPhysics :: [Rectangle] -> [Body] -> Time -> [Body]
runPhysics staticGeometry bodies 0 = bodies
runPhysics staticGeometry bodies dt = runPhysics' 0 staticGeometry bodies dt
  where
    runPhysics' steps staticGeometry bodies dt
      -- If there were no collisions, then we're done.
      | null collisions = integrateBodies bodies dt

      -- Sanity checks.
      | steps > 10  = error "runPhysics: too many steps"
      | tFirst == 0 = error "runPhysics: collision at t=0"

      -- If the first collision occurs after the time interval, then we're done.
      | tFirst > dt = integrateBodies bodies dt

      -- Otherwise, integrate the bodies until the time of the first collision.
      -- Resolve the collision, then run the simulation for the remainder of the
      -- time interval.
      | otherwise = runPhysics' (steps + 1) staticGeometry bodies' dt'

      where collisions = calculateCollisions staticGeometry bodies dt
            firstCollision = minimum collisions
            tFirst = collisionFirstContactTime firstCollision
            tFirst' = tFirst - epsilon
            dt' = dt - tFirst'
            epsilon = 1.0e-8
            nonCollidingBodies = bodies \\ collisionBodies firstCollision
            bodies' = resolveCollision firstCollision tFirst' ++ integrateBodies nonCollidingBodies tFirst'

-- Resolves the velocities of the bodies in the given collision.
--
-- Refer to chapter 7 of Game Physics Engine Development.
resolveCollision :: Collision -> Time -> [Body]
resolveCollision collision dt
  | numBodies == 1 = resolveOneBodyCollision collision dt
  | otherwise      = resolveTwoBodyCollision collision dt
  where numBodies = length $ collisionBodies collision

resolveOneBodyCollision :: Collision -> Time -> [Body]
resolveOneBodyCollision collision dt
  -- If the bodies are already separating, then there's no impulse required.
  | separatingVelocity > 0 = [body']

  -- If the body has infinite mass, then impulses have no effect.
  | inverseMass <= 0 = [body']

  -- Otherwise, apply an impulse proportional to the inverse mass of the bodies.
  | otherwise = [body' {bodyVelocity = velocity ^+^ totalInverseMass *^ impulsePerUnitInverseMass}]

  where body = head $ collisionBodies collision
        body' = integrateBody body dt
        separatingVelocity = velocity <.> collisionNormal collision
        separatingVelocity' = -restitution * separatingVelocity
        deltaVelocity = separatingVelocity' - separatingVelocity
        totalInverseMass = inverseMass
        impulse = deltaVelocity / totalInverseMass
        impulsePerUnitInverseMass = collisionNormal collision ^* impulse
        velocity = bodyVelocity body
        inverseMass = bodyInverseMass body
        restitution = 0.0

resolveTwoBodyCollision :: Collision -> Time -> [Body]
resolveTwoBodyCollision collision dt
  -- If the bodies are already separating, then there's no impulse required.
  | separatingVelocity > 0 = [bodyA', bodyB']

  -- If the bodies have infinite mass, then impulses have no effect.
  | totalInverseMass <= 0 = [bodyA', bodyB']

  -- Otherwise, apply an impulse proportional to the inverse mass of the bodies.
  | otherwise = [
                  bodyA' {bodyVelocity = velocityA ^+^ inverseMassA *^ impulsePerUnitInverseMass}
                , bodyB' {bodyVelocity = velocityB ^-^ inverseMassB *^ impulsePerUnitInverseMass}
                ]

  where [bodyA, bodyB] = collisionBodies collision
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

-- Returns the collisions between the given dynamic bodies over a time interval.
calculateCollisions :: [Rectangle] -> [Body] -> Time -> [Collision]
calculateCollisions staticGeometry bodies dt = staticCollisions ++ dynamicCollisions
  where
    -- Collide the dynamic bodies with the static geometry.
    staticCollisions = catMaybes [collideBodyWithStaticGeometry body rectangle | body <- bodies, rectangle <- staticGeometry]

    -- Collide the dynamic bodies with each other.
    dynamicCollisions = mapMaybe (uncurry collideBodyWithBody) $ pairs bodies

-- Returns a possible collision between the given body and static geometry.
collideBodyWithStaticGeometry :: Body -> Rectangle -> Maybe Collision
collideBodyWithStaticGeometry body rectangle
  | isJust contact = Just collision
  | otherwise      = Nothing
  where contact = calculateCollision a b velocityA zeroV
        a = AABB positionA extentsA
        b = getRectangleAABB rectangle
        Just (tFirst, tLast) = contact
        collision = Collision [body] tFirst tLast normal
        normal = normalized $ positionA ^-^ positionB
        velocityA = bodyVelocity body
        positionA = bodyPosition body
        extentsA = bodyExtents body
        positionB = aabbCentre b

-- Returns a possible collision between the given two bodies.
collideBodyWithBody :: Body -> Body -> Maybe Collision
collideBodyWithBody bodyA bodyB
  | isJust contact = Just collision
  | otherwise      = Nothing
  where contact = calculateCollision a b velocityA velocityB
        a = AABB positionA extentsA
        b = AABB positionB extentsB
        Just (tFirst, tLast) = contact
        collision = Collision [bodyA, bodyB] tFirst tLast normal
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

        ((aMin0, aMin1), (aMax0, aMax1)) = axisExtents a
        ((bMin0, bMin1), (bMax0, bMax1)) = axisExtents b

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

axisExtents :: AABB -> (Extents, Extents)
axisExtents (AABB centre extents) = (centre ^-^ extents, centre ^+^ extents)

-- Returns true if the given AABBs are intersecting, false otherwise.
intersectAABB :: AABB -> AABB -> Bool
intersectAABB (AABB (aCentre0, aCentre1) (aRadius0, aRadius1)) (AABB (bCentre0, bCentre1) (bRadius0, bRadius1))
  | abs (aCentre0 - bCentre0) > (aRadius0 + bRadius0) = False
  | abs (aCentre1 - bCentre1) > (aRadius1 + bRadius1) = False
  | otherwise = True

-- Converts the given rectangle to an AABB.
getRectangleAABB :: Rectangle -> AABB
getRectangleAABB (Rectangle position extents) = AABB position' extents'
  where position' = position ^+^ extents'
        extents'  = extents ^/ 2

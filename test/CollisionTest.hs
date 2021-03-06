module CollisionTest where

import Approx
import Test.Framework (buildTest, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Collision

main = defaultMain tests

tests = [testRunPhysics, testCalculateCollision, testIntersectAABB]

testRunPhysics = testGroup "runPhysics"
  [ testCase "test1" test1
  , testCase "test2" test2
  , testCase "test3" test3
  , testCase "test4" test4
  ]

  where a = Body (2, 0) (0.5, 0.5) ( 1, 0) 0 1.5
        b = Body (4, 0) (0.5, 0.5) (-1, 0) 0 0.5

        result = runPhysics [a, b] 1

        test1 = bodyPosition (head result) @?~= ( 1.5, 0.0)
        test2 = bodyPosition (last result) @?~= ( 3.5, 0.0)
        test3 = bodyVelocity (head result) @?~= (-2.0, 0.0)
        test4 = bodyVelocity (last result) @?~= ( 0.0, 0.0)

testCalculateCollision = testGroup "calculateCollision"
  [ testCase "test1" test1
  , testCase "test2" test2
  , testCase "test3" test3
  , testCase "test4" test4
  , testCase "test5" test5
  , testCase "test6" test6
  , testCase "test7" test7 ]

  where a = AABB (1, 0) (0.5, 0.5)
        b = AABB (2, 0) (0.5, 0.5)
        c = AABB (4, 0) (0.5, 0.5)
        d = AABB (4, 4) (0.5, 0.5)

        -- Initially overlapping.
        test1 = calculateCollision a b (1, 0) (0, 0) @?= Just (0, 0)

        -- Not moving.
        test2 = calculateCollision b c (0, 0) (0, 0) @?= Nothing

        -- Not moving fast enough.
        test3 = calculateCollision b c (0.25, 0) (-0.25, 0) @?= Nothing

        -- Moving in the opposite direction.
        test4 = calculateCollision b c (-2, 0) (0, 0) @?= Nothing

        -- Collision at half of the way.
        test5 = calculateCollision b c (1, 0) (-1, 0) @?= Just (0.5, 1)

        -- Collision at quarter of the way.
        test6 = calculateCollision b c (2, 0) (-2, 0) @?= Just (0.25, 0.75)

        -- Not in path.
        test7 = calculateCollision b d (1, 2) (0, 0) @?= Nothing

testIntersectAABB = testGroup "intersectAABB"
  [ testCase "test1" test1
  , testCase "test2" test2
  , testCase "test3" test3 ]

  where a = AABB (1, 0) (0.5, 0.5)
        b = AABB (2, 0) (0.5, 0.5)
        c = AABB (4, 0) (0.5, 0.5)

        test1 = intersectAABB a b @?= True
        test2 = intersectAABB b c @?= False
        test3 = intersectAABB a c @?= False

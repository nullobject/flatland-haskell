module CollisionTest where

import Test.Framework (buildTest, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Collision

main = defaultMain tests

tests = [testCalculateCollisions, testIntersectAABB]

testCalculateCollisions = testGroup "calculateCollisions"
  [ testCase "test1"  test1
  , testCase "test2"  test2
  , testCase "test3"  test3
  , testCase "test4"  test4
  , testCase "test5"  test5
  , testCase "test6"  test6
  , testCase "test7"  test7
  , testCase "test8"  test8
  , testCase "test9"  test9
  , testCase "test10" test10
  , testCase "test11" test11 ]

  where a = AABB (1, 0) (0.5, 0.5)
        b = AABB (2, 0) (0.5, 0.5)
        c = AABB (4, 0) (0.5, 0.5)
        d = AABB (4, 4) (0.5, 0.5)

        -- Initially overlapping.
        test1 = calculateCollisions a b (1, 0) (0, 0) @?= Just (Contact 0 0)

        -- Not moving.
        test2 = calculateCollisions b c (0, 0) (0, 0) @?= Nothing

        -- Not moving fast enough.
        test3 = calculateCollisions b c (0.5, 0) (0, 0) @?= Nothing

        -- Moving in the opposite direction.
        test4 = calculateCollisions b c (-2, 0) (0, 0) @?= Nothing

        -- Contact at half of the way.
        test5 = calculateCollisions b c (2, 0) (0, 0) @?= Just (Contact 0.5 1)

        -- Contact at quarter of the way.
        test6 = calculateCollisions b c (4, 0) (0, 0) @?= Just (Contact 0.25 0.75)

        -- Not moving fast enough.
        test7 = calculateCollisions a c (0, 0) (-0.5, 0) @?= Nothing

        -- Moving in the opposite direction.
        test8 = calculateCollisions a c (0, 0) (4, 0) @?= Nothing

        -- Contact at half of the way.
        test9 = calculateCollisions a c (0, 0) (-4, 0) @?= Just (Contact 0.5 1)

        -- Contact at quarter of the way.
        test10 = calculateCollisions a c (0, 0) (-8, 0) @?= Just (Contact 0.25 0.5)

        -- Not in path.
        test11 = calculateCollisions b d (2, 0) (0, 0) @?= Nothing

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

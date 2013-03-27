module CollisionTest where

import Test.Framework (buildTest, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Collision

main = defaultMain tests

tests = [testCalculateCollisions, testIntersectAABB]

testCalculateCollisions = testGroup "calculateCollisions"
  [ testCase "test1" test1
  , testCase "test2" test2
  , testCase "test3" test3
  , testCase "test4" test4 ]

  where a = AABB (1, 0) (0.5, 0.5)
        b = AABB (2, 0) (0.5, 0.5)
        c = AABB (4, 0) (0.5, 0.5)

        test1 = calculateCollisions a b (1, 0) (0, 0) @?= Just (Contact 0 0)
        test2 = calculateCollisions b c (2, 0) (0, 0) @?= Just (Contact 0.5 1)
        test3 = calculateCollisions b c (4, 0) (0, 0) @?= Just (Contact 0.25 0.75)
        test4 = calculateCollisions a c (1, 0) (0, 0) @?= Nothing

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

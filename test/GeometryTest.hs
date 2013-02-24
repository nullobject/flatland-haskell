module GeometryTest where

import Test.Framework (buildTest, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Geometry

main = defaultMain tests

tests = [testAngleBetween, testPointSegmentDistance, testIntersectLines]

testAngleBetween = testGroup "angleBetween"
  [ testCase "test1" test1
  , testCase "test2" test2 ]
  where test1 = angleBetween (1, 0) (1, 1) @?= pi / 2
        test2 = angleBetween (0, 1) (1, 1) @?= 0

testPointSegmentDistance = testGroup "pointSegmentDistance"
  [ testCase "test1" test1
  , testCase "test2" test2
  , testCase "test3" test3
  , testCase "test4" test4
  , testCase "test5" test5
  , testCase "test6" test6 ]
  where s = Segment (0, 0) (1, 1)
        test1 = pointSegmentDistance (0.0, 0.0) s @?= 0
        test2 = pointSegmentDistance (1.0, 1.0) s @?= 0
        test3 = pointSegmentDistance (1.0, 0.0) s @?= 0.7071067811865476
        test4 = pointSegmentDistance (1.0, 0.5) s @?= 0.3535533905932738
        test5 = pointSegmentDistance (0.0, 1.0) s @?= 0.7071067811865476
        test6 = pointSegmentDistance (0.5, 1.0) s @?= 0.3535533905932738

testIntersectLines = testGroup "intersectLines"
  [ testCase "test1" test1
  , testCase "test2" test2
  , testCase "test3" test3
  , testCase "test4" test4 ]
  where test1 = intersectLines (-1, -1) (1,  1) (-1, 1) (1, -1) @?= ( 0,  0)
        test2 = intersectLines (-1,  1) (1,  1) (-1, 1) (1, -1) @?= (-1,  1)
        test3 = intersectLines (-1, -1) (1, -1) (-1, 1) (1, -1) @?= ( 1, -1)
        test4 = intersectLines (-1, -1) (1,  1) (-1, 1) (1,  1) @?= ( 1,  1)

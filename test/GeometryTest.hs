module GeometryTest where

import Approx
import Test.Framework (buildTest, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Geometry

main = defaultMain tests

tests = [testAngleBetween, testPointSegmentDistance, testIntersectLines, testIntersectTriangle]

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

        test1 = pointSegmentDistance (0.0, 0.0) s @?~= 0.00
        test2 = pointSegmentDistance (1.0, 1.0) s @?~= 0.00
        test3 = pointSegmentDistance (1.0, 0.0) s @?~= 0.71
        test4 = pointSegmentDistance (1.0, 0.5) s @?~= 0.35
        test5 = pointSegmentDistance (0.0, 1.0) s @?~= 0.71
        test6 = pointSegmentDistance (0.5, 1.0) s @?~= 0.35

testIntersectLines = testGroup "intersectLines"
  [ testCase "test1" test1
  , testCase "test2" test2
  , testCase "test3" test3
  , testCase "test4" test4 ]

  where test1 = intersectLines (-1, -1) (1,  1) (-1, 1) (1, -1) @?= ( 0,  0)
        test2 = intersectLines (-1,  1) (1,  1) (-1, 1) (1, -1) @?= (-1,  1)
        test3 = intersectLines (-1, -1) (1, -1) (-1, 1) (1, -1) @?= ( 1, -1)
        test4 = intersectLines (-1, -1) (1,  1) (-1, 1) (1,  1) @?= ( 1,  1)

testIntersectTriangle = testGroup "intersectTriangle"
  [ testCase "test1" test1
  , testCase "test2" test2
  , testCase "test3" test3
  , testCase "test4" test4
  , testCase "test5" test5
  , testCase "test6" test6
  , testCase "test7" test7
  , testCase "test8" test8
  , testCase "test9" test9 ]

  where triangle = Triangle (0, 0) (1, 1) (1, 0)

        test1 = intersectTriangle ( 0,  0) triangle @?= True
        test2 = intersectTriangle ( 1,  1) triangle @?= True
        test3 = intersectTriangle ( 1,  0) triangle @?= True
        test4 = intersectTriangle ( 0, -1) triangle @?= False
        test5 = intersectTriangle (-1,  0) triangle @?= False
        test6 = intersectTriangle ( 1,  2) triangle @?= False
        test7 = intersectTriangle ( 2,  1) triangle @?= False
        test8 = intersectTriangle ( 2,  0) triangle @?= False
        test9 = intersectTriangle ( 1, -1) triangle @?= False

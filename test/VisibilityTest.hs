module VisibilityTest where

import Test.Framework (buildTest, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Geometry
import Visibility

main = defaultMain tests

tests = [testCalculateEndpoints, testSweep]

testCalculateEndpoints = testGroup "calculateEndpoints"
  [ testCase "test1" test1
  , testCase "test2" test2 ]

  where segment = Segment (-1, 1) (1, 1)

        test1 = calculateEndpoints (0, 0) segment @?=
          [ Endpoint (-1, 1) (Segment (-1, 1) (1, 1)) 2.356194490192345  False
          , Endpoint ( 1, 1) (Segment (-1, 1) (1, 1)) 0.7853981633974483 True ]

        test2 = calculateEndpoints (0, 1) segment @?=
          [ Endpoint (-1, 1) (Segment (-1, 1) (1, 1)) 3.141592653589793 False
          , Endpoint ( 1, 1) (Segment (-1, 1) (1, 1)) 0.0               True ]

testSweep = testGroup "calculateVisibility"
  [ testCase "test1" test1
  , testCase "test2" test2
  , testCase "test3" test3
  , testCase "test4" test4 ]

  where polygons = [ Polygon [(-1,  1), ( 1,  1), ( 1, -1), (-1, -1)]
                   , Polygon [(-5,  5), ( 5,  5), ( 5, -5), (-5, -5)] ]

        test1 = calculateVisibility (0, 0) polygons @?=
          [ Triangle (0, 0) ( 1,  1) (-1,  1)
          , Triangle (0, 0) ( 1, -1) ( 1,  1)
          , Triangle (0, 0) (-1, -1) ( 1, -1)
          , Triangle (0, 0) (-1,  1) (-1, -1) ]

        test2 = calculateVisibility (0, 1) polygons @?=
          [ Triangle (0, 1) ( 1,  1) (-1,  1)
          , Triangle (0, 1) ( 1, -1) ( 1,  1)
          , Triangle (0, 1) (-1, -1) ( 1, -1)
          , Triangle (0, 1) (-1,  1) (-1, -1) ]

        test3 = calculateVisibility (0, 2) polygons @?=
          [ Triangle (0, 2) ( 5,  5) (-5,  5)
          , Triangle (0, 2) ( 5, -3) ( 5,  5)
          , Triangle (0, 2) (-1,  1) ( 1,  1)
          , Triangle (0, 2) (-5,  5) (-5, -3) ]

        test4 = calculateVisibility (0, 4) polygons @?=
          [ Triangle (0, 4) ( 5,  5) (-5,  5)
          , Triangle (0, 4) ( 5, -5) ( 5,  5)
          , Triangle (0, 4) ( 3, -5) ( 5, -5)
          , Triangle (0, 4) (-1,  1) ( 1,  1)
          , Triangle (0, 4) (-5, -5) (-3, -5)
          , Triangle (0, 4) (-5,  5) (-5, -5) ]

module VisibilityTest where

import Test.Framework (buildTest, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Geometry
import Visibility

main = defaultMain tests

tests = [testCalculateEndpoints, testCalculateVisibilityMesh]

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

testCalculateVisibilityMesh = testGroup "calculateVisibilityMesh"
  [ testCase "test1" test1
  , testCase "test2" test2
  , testCase "test3" test3
  , testCase "test4" test4 ]

  where rectangles = [ Rectangle (-1, -1) ( 2,  2)
                     , Rectangle (-5, -5) (10, 10) ]

        test1 = calculateVisibilityMesh (0, 0) rectangles @?=
          [ Triangle (0, 0) ( 1,  1) (-1,  1)
          , Triangle (0, 0) ( 1, -1) ( 1,  1)
          , Triangle (0, 0) (-1, -1) ( 1, -1)
          , Triangle (0, 0) (-1,  1) (-1, -1) ]

        test2 = calculateVisibilityMesh (0, 1) rectangles @?=
          [ Triangle (0, 1) (-1,  1) ( 1,  1)
          , Triangle (0, 1) ( 1, -1) ( 1,  1)
          , Triangle (0, 1) (-1, -1) ( 1, -1)
          , Triangle (0, 1) (-1,  1) (-1, -1) ]

        test3 = calculateVisibilityMesh (0, 2) rectangles @?=
          [ Triangle (0, 2) ( 5,  5) (-5,  5)
          , Triangle (0, 2) ( 5, -3) ( 5,  5)
          , Triangle (0, 2) (-1,  1) ( 1,  1)
          , Triangle (0, 2) (-5,  5) (-5, -3) ]

        test4 = calculateVisibilityMesh (0, 4) rectangles @?=
          [ Triangle (0, 4) ( 5,  5) (-5,  5)
          , Triangle (0, 4) ( 5, -5) ( 5,  5)
          , Triangle (0, 4) ( 3, -5) ( 5, -5)
          , Triangle (0, 4) (-1,  1) ( 1,  1)
          , Triangle (0, 4) (-5, -5) (-3, -5)
          , Triangle (0, 4) (-5,  5) (-5, -5) ]

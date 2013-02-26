module VisibilityTest where

import Test.Framework (buildTest, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Geometry
import Visibility

main = defaultMain tests

tests = [testCalculateEndpoints, testSweep]

testCalculateEndpoints = testGroup "calculateEndpoints"
  [ testCase "test1" test1 ]

  where segment = Segment (-1, 1) (1, 1)

        test1 = calculateEndpoints (0, 0) segment @?=
          [ Endpoint (-1, 1) (Segment (-1, 1) (1, 1)) 2.356194490192345  False
          , Endpoint ( 1, 1) (Segment (-1, 1) (1, 1)) 0.7853981633974483 True ]

testSweep = testGroup "calculateVisibility"
  [ testCase "test1" test1
  , testCase "test2" test2
  , testCase "test3" test3 ]

  where segments = [ Segment (-1,  1) ( 1,  1)
                   , Segment ( 1,  1) ( 1, -1)
                   , Segment ( 1, -1) (-1, -1)
                   , Segment (-1, -1) (-1,  1)
                   , Segment (-5,  5) ( 5,  5)
                   , Segment ( 5,  5) ( 5, -5)
                   , Segment ( 5, -5) (-5, -5)
                   , Segment (-5, -5) (-5,  5) ]

        test1 = map snap (calculateVisibility (0, 0) segments) @?=
          [ Triangle (0, 0) ( 1,  1) (-1,  1)
          , Triangle (0, 0) ( 1, -1) ( 1,  1)
          , Triangle (0, 0) (-1, -1) ( 1, -1)
          , Triangle (0, 0) (-1,  1) (-1, -1) ]

        test2 = map snap (calculateVisibility (0, 2) segments) @?=
          [ Triangle (0, 2) ( 5,  5) (-5,  5)
          , Triangle (0, 2) ( 5, -3) ( 5,  5)
          , Triangle (0, 2) (-1,  1) ( 1,  1)
          , Triangle (0, 2) (-5,  5) (-5, -3) ]

        test3 = map snap (calculateVisibility (0, 4) segments) @?=
          [ Triangle (0, 4) ( 5,  5) (-5,  5)
          , Triangle (0, 4) ( 5, -5) ( 5,  5)
          , Triangle (0, 4) ( 3, -5) ( 5, -5)
          , Triangle (0, 4) (-1,  1) ( 1,  1)
          , Triangle (0, 4) (-5, -5) (-3, -5)
          , Triangle (0, 4) (-5,  5) (-5, -5) ]

        snap (Triangle (a1, a2) (b1, b2) (c1, c2)) = Triangle (round' a1, round' a2) (round' b1, round' b2) (round' c1, round' c2)

        round' = fromInteger . round

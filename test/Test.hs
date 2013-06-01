module Main where

import Test.Framework (defaultMain)

import qualified CollisionTest
import qualified GeometryTest
import qualified VisibilityTest

main = defaultMain $ CollisionTest.tests ++ GeometryTest.tests ++ VisibilityTest.tests

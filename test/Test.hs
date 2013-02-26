module Main where

import Test.Framework (defaultMain)

import qualified GeometryTest
import qualified VisibilityTest

main = defaultMain $ GeometryTest.tests ++ VisibilityTest.tests

module WorldTest where

import Test.Framework (buildTest, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified World

main = defaultMain [WorldTest.test]

test = buildTest $ do
  let world = World.empty

  return $ testGroup "World" [
      testCase "incrementAge" $ testIncrementAge world
    ]

testIncrementAge world = do
  World.age world @?= 0
  let world' = World.incrementAge world
  World.age world' @?= 1

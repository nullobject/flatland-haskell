module EntityTest where

import Test.Framework (buildTest, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.UUID.V4 as UUID
import qualified Entity

main = defaultMain [EntityTest.test]

test = buildTest $ do
  uuid <- UUID.nextRandom
  let entity = Entity.empty uuid

  return $ testGroup "Entity" [
      testCase "tick" $ testTick entity
    , testCase "move" $ testMove entity
    ]

testTick entity = do
  Entity.age entity @?= 0
  let entity' = Entity.tick entity
  Entity.age entity' @?= 1

testMove entity = do
  Entity.position entity @?= (0, 0)
  let entity' = Entity.move entity
  Entity.position entity' @?= (1, 1)

module Main where

import Test.Framework (defaultMain)

import qualified EntityTest
import qualified WorldTest

main = defaultMain [EntityTest.test, WorldTest.test]

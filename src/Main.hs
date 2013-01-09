module Main where

import Control.Concurrent (forkIO)

import qualified Game
import qualified Server

main :: IO ()
main = do
  forkIO $ Game.run
  Server.run

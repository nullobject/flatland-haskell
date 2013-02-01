module Main where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM (newTChanIO)
import qualified Game
import qualified Server

main :: IO ()
main = do
  chan <- newTChanIO
  _ <- forkIO $ Game.run chan
  Server.run chan

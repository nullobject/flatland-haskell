module Main where

import           Control.Concurrent (forkIO, killThread, newChan)
import           Control.Concurrent.STM (newTChanIO)
import           Control.Exception (bracket)
import qualified Game
import qualified Server

main :: IO ()
main = do
  messageChannel <- newTChanIO
  eventChannel <- newChan
  bracket
    (forkIO $ Game.run messageChannel eventChannel)
    killThread
    (const $ Server.run eventChannel messageChannel)

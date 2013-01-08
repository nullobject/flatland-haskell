module Main where

import qualified Server
-- import Data.Aeson (encode)
-- import qualified Data.ByteString.Lazy.Char8 as B
-- import qualified Data.Map as Map
-- import qualified Player
-- import qualified World
-- import qualified WorldView

main :: IO ()
main = Server.run

-- initWorld :: World.WorldState ()
-- initWorld = World.spawn Player.empty

-- loop :: World.WorldState ()
-- loop = do
--   world <- get
--   let uuid = head $ Map.keys $ World.players world
--   World.move uuid
--   World.tick
--   liftIO $ B.putStrLn $ encode $ WorldView.fromWorld world
--   world' <- get
--   when (World.age world' < 10) loop

-- main :: IO ()
-- main = do
--   world <- execStateT initWorld World.empty
--   _     <- execStateT loop world
--   return ()

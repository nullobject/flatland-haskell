import Control.Monad.State
import Player
import World

loop = do
  spawn initPlayer
  spawn initPlayer
  World.tick

main = do
  s <- execStateT loop initWorld
  print (show s)

import Control.Monad.State
import Player
import World

loop :: WorldState
loop = lift initPlayer >>= spawn >>
       lift initPlayer >>= spawn >>
       World.tick

main = do
  world <- initWorld
  state <- execStateT loop world
  print (show state)

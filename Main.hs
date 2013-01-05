import Control.Monad.State
import Player
import World

loop :: StateT World IO ()
loop = lift initPlayer >>= spawn >>
       lift initPlayer >>= spawn >>
       World.tick

main = do
  s <- execStateT loop initWorld
  print (show s)

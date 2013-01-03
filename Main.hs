import Control.Monad.State
import Player
import World

main = do
  s <- runStateT (do spawn initPlayer; spawn initPlayer; World.tick) initWorld
  putStrLn (show s)

import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.UUID (fromString)
import qualified Player
import qualified World

initWorld :: World.WorldState ()
initWorld = lift Player.empty >>= World.spawn >>
            lift Player.empty >>= World.spawn

loop :: World.WorldState ()
loop = do
  world <- get
  let uuid = head $ Map.keys $ World.players world
  World.move uuid
  World.tick

main :: IO ()
main = World.empty >>=
       execStateT initWorld >>=
       execStateT loop >>=
       print

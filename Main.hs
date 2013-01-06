import Control.Monad.State
import Data.Aeson (encode)
import qualified Data.Map as Map
import qualified Player
import qualified World
import qualified WorldView

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
       \world -> print $ encode $ WorldView.fromWorld world

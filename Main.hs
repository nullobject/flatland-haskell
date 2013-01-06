import Control.Monad.State
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as Map
import qualified Player
import System.Exit
import qualified World
import qualified WorldView

initWorld :: World.WorldState ()
initWorld = lift Player.empty >>= World.spawn >>
            lift Player.empty >>= World.spawn

loop :: World.WorldState ()
loop = forever $ do
  world <- get
  when (World.age world >= 10) (lift exitSuccess)
  let uuid = head $ Map.keys $ World.players world
  World.move uuid
  World.tick
  liftIO $ B.putStrLn $ encode $ WorldView.fromWorld world

main :: IO ()
main = World.empty >>=
       execStateT initWorld >>=
       execStateT loop >>=
       print

import Control.Monad.State
import Data.Maybe (fromJust)
import Data.UUID (fromString)
import qualified Player
import qualified World

initState :: World.WorldState ()
initState = lift Player.empty >>= World.spawn >>
            lift Player.empty >>= World.spawn

loop :: World.WorldState ()
loop = World.move (fromJust (fromString "acb42012-4d0f-47dd-a541-b6f949f4066c")) >> World.tick

main :: IO ()
main = World.empty >>=
       execStateT initState >>=
       execStateT loop >>=
       print

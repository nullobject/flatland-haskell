{-# LANGUAGE Rank2Types #-}

module Server where

import           Control.Concurrent.STM (TChan)
import           Control.Monad.State
import           Data.Aeson (encode, ToJSON)
import           Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy as LBS
import           Data.Conduit (ResourceT)
import qualified Data.Maybe as Maybe
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Network.HTTP.Types (status200, status400)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Types

data Server = Server {
  chan :: TChan Request
}

type ServerState = StateT Server (ResourceT IO)

responseFailure :: LBS.ByteString -> Wai.Response
responseFailure value = Wai.responseLBS status400 [("Content-Type", "text/plain")] value

responseSuccess :: (ToJSON a) => a -> Wai.Response
responseSuccess value = Wai.responseLBS status200 [("Content-Type", "application/json")] $ encode $ value

getPlayer :: Wai.Request -> ServerState (Maybe UUID)
getPlayer request = do
  let player = lookup "X-Player" $ Wai.requestHeaders request
  return $ player >>= UUID.fromString . unpack

-- TODO: Parse the different messages.
actionHandler :: Wai.Request -> ServerState Wai.Response
actionHandler request = do
  getPlayer request >>= Maybe.maybe failure success
  where
    failure = return $ responseFailure "Missing header X-Player"
    success uuid = do
      server <- get
      let message = ActionMessage Idle uuid
      WorldViewMessage worldView <- liftIO $ chan server `ask` message
      return $ responseSuccess worldView

route :: Wai.Request -> ServerState Wai.Response
route request =
  case Wai.pathInfo request of
    []         -> return $ Wai.ResponseFile status200 [("Content-Type", "text/html")] "static/index.html" Nothing
    ["action"] -> actionHandler request
    _          -> error "unexpected pathInfo"

-- Runs the server with the given request channel.
run :: TChan Request -> IO ()
run chan = Warp.run 8000 app
  where app request = evalStateT (route request) (Server chan)

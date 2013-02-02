{-# LANGUAGE Rank2Types #-}

module Server where

import           Action
import           Control.Monad.State
import           Core
import           Data.Aeson (encode, ToJSON)
import           Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy as LBS
import           Data.Conduit (ResourceT)
import qualified Data.Maybe as Maybe
import           Identifier
import           Network.HTTP.Types (status200, status400)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Request
import           WorldView (WorldView)

data ServerState = ServerState {
  chan :: Channel Message WorldView
}

type Server = StateT ServerState (ResourceT IO)

responseFailure :: LBS.ByteString -> Wai.Response
responseFailure value = Wai.responseLBS status400 [("Content-Type", "text/plain")] value

responseSuccess :: (ToJSON a) => a -> Wai.Response
responseSuccess value = Wai.responseLBS status200 [("Content-Type", "application/json")] $ encode $ value

getPlayer :: Wai.Request -> Server (Maybe Identifier)
getPlayer request = do
  let player = lookup "X-Player" $ Wai.requestHeaders request
  return $ player >>= readMaybe . unpack

-- TODO: Parse the different messages.
actionHandler :: Wai.Request -> Server Wai.Response
actionHandler request = do
  getPlayer request >>= Maybe.maybe failure success
  where
    failure = return $ responseFailure "Missing header X-Player"
    success identifier = do
      server <- get
      let message = (identifier, Idle)
      worldView <- liftIO $ chan server `ask` message
      return $ responseSuccess worldView

route :: Wai.Request -> Server Wai.Response
route request =
  case Wai.pathInfo request of
    []         -> return $ Wai.ResponseFile status200 [("Content-Type", "text/html")] "static/index.html" Nothing
    ["action"] -> actionHandler request
    _          -> error "unexpected pathInfo"

-- Runs the server with the given request channel.
run :: Channel Message WorldView -> IO ()
run chan' = Warp.run 8000 app
  where
    app request = evalStateT (route request) (ServerState chan')

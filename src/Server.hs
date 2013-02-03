{-# LANGUAGE Rank2Types #-}

module Server where

import           Action
import           Channel (ask, Channel)
import           Control.Applicative ((<$>))
import           Control.Monad.State
import           Core
import           Data.Aeson (decode, encode, ToJSON)
import           Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Conduit as C
import qualified Data.Conduit.Lazy as LC
import qualified Data.Maybe as Maybe
import           Identifier
import           Network.HTTP.Types (status200, status400)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           WorldView (WorldView)

data ServerState = ServerState {
  chan :: Channel Message WorldView
}

type Server = StateT ServerState (C.ResourceT IO)

requestBody :: Wai.Request -> Server LBS.ByteString
requestBody request = lift $ LBS.fromChunks <$> LC.lazyConsume requestBody
  where requestBody = Wai.requestBody request

responseFailure :: LBS.ByteString -> Wai.Response
responseFailure value = Wai.responseLBS status400 [("Content-Type", "text/plain")] value

responseSuccess :: (ToJSON a) => a -> Wai.Response
responseSuccess value = Wai.responseLBS status200 [("Content-Type", "application/json")] $ encode $ value

getPlayer :: Wai.Request -> Server (Maybe Identifier)
getPlayer request = do
  let player = lookup "X-Player" $ Wai.requestHeaders request
  return $ player >>= readMaybe . unpack

actionHandler :: Wai.Request -> Server Wai.Response
actionHandler request = do
  getPlayer request >>= Maybe.maybe failure success
  where
    failure = return $ responseFailure "Missing header X-Player"
    success identifier = do
      body <- requestBody request
      let action = decode body :: Maybe Action
      liftIO $ print $ action
      server <- get
      let message = (identifier, Maybe.fromJust action)
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
run chan = Warp.run 8000 app
  where
    app request = evalStateT (route request) (ServerState chan)

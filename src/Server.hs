{-# LANGUAGE Rank2Types #-}

module Server where

import           Action
import           Channel (ask, Channel)
import           Control.Applicative ((<$>))
import           Control.Concurrent (Chan)
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
import qualified Network.Wai.EventSource as Wai.EventSource
import qualified Network.Wai.Handler.Warp as Warp
import           World (World)
import           WorldView (WorldView)

data ServerState = ServerState
  { eventChannel   :: Chan Wai.EventSource.ServerEvent
  , messageChannel :: Channel Message WorldView
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
      server <- get
      body <- requestBody request
      let action = decode body :: Maybe Action
      let message = (identifier, Maybe.fromJust action)
      liftIO $ print $ message
      worldView <- liftIO $ messageChannel server `ask` message
      return $ responseSuccess worldView

route :: Wai.Request -> Server Wai.Response
route request = do
  server <- get
  case Wai.pathInfo request of
    []                -> return $ Wai.ResponseFile status200 [("Content-Type", "text/html")]       "static/index.html"   Nothing
    ["d3.v3.min.js"]  -> return $ Wai.ResponseFile status200 [("Content-Type", "text/javascript")] "static/d3.v3.min.js" Nothing
    ["flatland.js"]   -> return $ Wai.ResponseFile status200 [("Content-Type", "text/javascript")] "static/flatland.js"  Nothing
    ["flatland.css"]  -> return $ Wai.ResponseFile status200 [("Content-Type", "text/css")]        "static/flatland.css" Nothing
    ["tiles.png"]     -> return $ Wai.ResponseFile status200 [("Content-Type", "image/png")]       "static/tiles.png"    Nothing
    ["action"]        -> actionHandler request
    ["events"]        -> lift $ Wai.EventSource.eventSourceAppChan (eventChannel server) request
    _                 -> error "unexpected pathInfo"

-- Runs the server with the given request channel.
run :: Chan Wai.EventSource.ServerEvent -> Channel Message WorldView -> IO ()
run eventChannel messageChannel = Warp.runSettings settings app
  where app request = evalStateT (route request) (ServerState eventChannel messageChannel)
        settings = Warp.defaultSettings
          { Warp.settingsPort            = 8000
          , Warp.settingsFdCacheDuration = 0
          }

{-# LANGUAGE Rank2Types #-}

module Server where

import           Control.Concurrent.STM (TChan)
import           Control.Monad.Trans (liftIO)
import           Control.Monad.Trans (liftIO)
import           Data.Aeson (encode, ToJSON)
import           Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Maybe as Maybe
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Network.HTTP.Types (status200, status400)
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Types

returnSuccess :: (Monad m, ToJSON a) => a -> m Response
returnSuccess value = return $ responseLBS status200 [("Content-Type", "application/json")] $ encode $ value

returnFailure :: Monad m => LBS.ByteString -> m Response
returnFailure value = return $ responseLBS status400 [("Content-Type", "text/plain")] value

getPlayer :: Request -> IO (Maybe UUID)
getPlayer request = do
  let player = lookup "X-Player" $ requestHeaders request
  return $ player >>= UUID.fromString . unpack

actionHandler :: TChan GameRequest -> Application
actionHandler chan request = do
  liftIO $ getPlayer request >>= Maybe.maybe fail ok
  where
    ok uuid = do
      let message = ActionMessage Move uuid
      WorldViewMessage worldView <- liftIO $ chan `ask` message
      returnSuccess worldView
    fail = returnFailure "Missing header X-Player"

application :: TChan GameRequest -> Application
application chan request =
  case pathInfo request of
    []         -> return $ ResponseFile status200 [("Content-Type", "text/html")] "static/index.html" Nothing
    ["action"] -> actionHandler chan request
    _          -> error "unexpected pathInfo"

run :: TChan GameRequest -> IO ()
run chan = Warp.run 8000 $ application chan

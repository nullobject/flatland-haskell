{-# LANGUAGE Rank2Types #-}

module Server where

import           Control.Concurrent.STM (TChan)
import           Control.Monad.Trans (liftIO)
import           Data.Aeson (encode)
import           Data.ByteString.UTF8 (toString)
import qualified Data.Maybe as Maybe
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Network.HTTP.Types (status200)
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Types

getPlayer :: Request -> IO (Maybe UUID)
getPlayer request = do
  let player = lookup "X-Player" $ requestHeaders request
  return $ UUID.fromString $ toString $ Maybe.fromJust player

-- FIXME: Error unless the player is given.
actionHandler :: TChan GameRequest -> Application
actionHandler chan request = do
  uuid <- liftIO $ getPlayer request
  let message = ActionMessage Move $ Maybe.fromJust uuid
  WorldViewMessage worldView <- liftIO $ chan `ask` message
  return $ responseLBS status200 [("Content-Type", "application/json")] $ encode $ worldView

application :: TChan GameRequest -> Application
application chan request =
  case pathInfo request of
    []         -> return $ ResponseFile status200 [("Content-Type", "text/html")] "static/index.html" Nothing
    ["action"] -> actionHandler chan request
    _          -> error "unexpected pathInfo"

run :: TChan GameRequest -> IO ()
run chan = Warp.run 8000 $ application chan

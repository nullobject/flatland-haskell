{-# LANGUAGE Rank2Types #-}

module Server where

import           Blaze.ByteString.Builder (Builder)
import           Blaze.ByteString.Builder.Char.Utf8 (fromString)
import           Control.Applicative
import           Control.Concurrent (threadDelay)
import           Control.Monad.State (liftIO)
import           Data.Aeson (encode)
import           Data.ByteString.UTF8 (toString)
import           Data.Enumerator.List (repeatM)
import qualified Data.Maybe as Maybe
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Snap.Core hiding (Request)
import           Snap.Http.Server
import           Snap.Util.FileServe
import           System.Random
import           Types

getPlayer :: Snap (Maybe UUID)
getPlayer = do
  playerHeader <- getsRequest $ getHeader "X-Player"
  return $ UUID.fromString $ toString $ Maybe.fromJust playerHeader

-- FIXME: Error unless the player is given.
actionHandler :: RequestChan -> Snap ()
actionHandler chan = do
  uuid <- getPlayer
  respond chan $ Maybe.fromJust uuid

respond :: RequestChan -> UUID -> Snap ()
respond chan uuid = do
  let message = ActionMessage Move uuid
  WorldViewMessage worldView <- liftIO $ chan `ask` message
  writeLBS $ encode $ worldView

streamHandler :: (forall a . Enumerator Builder IO a) -> Snap ()
streamHandler enum = modifyResponse $
  setBufferingMode False .
  setResponseBody  enum .
  setContentType   "text/event-stream" .
  setHeader        "Access-Control-Allow-Origin" "*" .
  setHeader        "Cache-Control" "no-cache" .
  setHeader        "Connection" "Keep-Alive"

streamEnumerator :: Enumerator Builder IO a
streamEnumerator = repeatM $ do
  n <- liftIO $ threadDelay oneSecond >> randomIO :: IO Int
  return $ fromString $ "data: " ++ (show n) ++ "\n\n"

site :: RequestChan -> Snap ()
site chan = route [ ("action", actionHandler chan)
                  , ("stream", streamHandler streamEnumerator) ]
  <|> dir "static" (serveDirectory "www")

run :: RequestChan -> IO ()
run chan = quickHttpServe $ site chan

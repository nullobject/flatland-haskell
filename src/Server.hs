{-# LANGUAGE Rank2Types #-}

module Server where

import           Blaze.ByteString.Builder (Builder)
import           Blaze.ByteString.Builder.Char.Utf8 (fromString)
import           Control.Applicative
import           Control.Concurrent (threadDelay)
import           Control.Monad.State (liftIO)
import           Data.Aeson (encode)
import           Data.Enumerator.List (repeatM)
import qualified Data.Maybe as Maybe
import qualified Data.UUID as UUID
import           Snap.Core hiding (Request)
import           Snap.Http.Server
import           Snap.Util.FileServe
import           System.Random
import           Types

-- FIXME: Don't fake the uuid.
actionHandler :: RequestChan -> Snap ()
actionHandler chan = do
  let uuid = Maybe.fromJust $ UUID.fromString "d2141ba3-3461-41c0-a5d5-8160f524a47b"
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

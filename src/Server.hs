{-# LANGUAGE Rank2Types #-}

module Server where

import           Blaze.ByteString.Builder (Builder)
import           Blaze.ByteString.Builder.Char.Utf8 (fromString)
import           Control.Applicative
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM
import           Control.Monad.State (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Enumerator.List (repeatM)
import           Snap.Core
import           Snap.Http.Server
import           Snap.Util.FileServe
import           System.Random

oneSecond :: Int
oneSecond = 1000000

ask :: TChan (TMVar a) -> IO a
ask chan = do
  mvar <- newEmptyTMVarIO
  atomically $ writeTChan chan mvar
  atomically $ takeTMVar mvar

actionHandler :: TChan (TMVar B.ByteString) -> Snap ()
actionHandler chan = do
  response <- liftIO $ ask chan
  writeLBS response

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
  n <- liftIO $ randomIO :: IO Int
  liftIO $ threadDelay oneSecond
  return $ fromString $ "data: " ++ (show n) ++ "\n\n"

site :: TChan (TMVar B.ByteString) -> Snap ()
site chan = route [ ("action", actionHandler chan)
                  , ("stream", streamHandler streamEnumerator) ]
  <|> dir "static" (serveDirectory "www")

run :: TChan (TMVar B.ByteString) -> IO ()
run chan = quickHttpServe $ site chan

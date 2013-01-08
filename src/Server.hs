{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Server where

import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad.State (liftIO)
import Data.Enumerator.List (repeatM)
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import System.Random

oneMillisecond :: Int
oneMillisecond = 1000

streamResponse :: (forall a . Enumerator Builder IO a) -> Snap ()
streamResponse enum = modifyResponse $
  setBufferingMode False .
  setResponseBody  enum .
  setContentType   "text/event-stream" .
  setHeader        "Access-Control-Allow-Origin" "*" .
  setHeader        "Cache-Control" "no-cache" .
  setHeader        "Connection" "Keep-Alive"

streamEnumerator :: Enumerator Builder IO a
streamEnumerator = repeatM $ do
  n <- liftIO $ randomIO :: IO Int
  liftIO $ threadDelay delay
  return $ fromString $ "data: " ++ (show n) ++ "\n\n"
  where delay = oneMillisecond * 100

site :: Snap ()
site =
  route [("stream", streamResponse streamEnumerator)] <|>
  dir "static" (serveDirectory "www")

run :: IO ()
run = quickHttpServe site

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module HTTP where

import           Data.Aeson           (ToJSON)
import           Data.ByteString.Lazy (ByteString)
import           Data.Function        ((&))
import           Network.HTTP.Simple  hiding (Response)

import           Tracer


data Response a = Response a


class Monad m => MonadHTTP m where
  get :: String -> m (Response ByteString)
  post :: ToJSON a => String -> a -> m (Response ByteString)

instance MonadHTTP IO where
  get url = do
    request <- parseRequest url
    response <- httpLBS request
    pure . Response . getResponseBody $ response

  post url payload = do
    request' <- parseRequest url
    let request =
          request'
          & setRequestMethod "POST"
          & setRequestBodyJSON payload
    response <- httpLBS request
    pure . Response . getResponseBody $ response

instance MonadHTTP Tracer where
  get url =
    trace "HTTP" ("GET request to " <> url) >> pure (Response "")

  post url _payload =
    trace "HTTP" ("POST request to " <> url) >> pure (Response "")

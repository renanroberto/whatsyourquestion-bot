{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module HTTP where

import           Data.Aeson           (ToJSON)
import           Data.ByteString.Lazy (ByteString)
import           Data.Function        ((&))
import           Network.HTTP.Simple  hiding (Response)

import           Logger


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

instance MonadHTTP (Logger [String]) where
  get url =
    logger "HTTP" ("GET request to " <> url) >> pure (Response "")

  post url _payload =
    logger "HTTP" ("POST request to " <> url) >> pure (Response "")

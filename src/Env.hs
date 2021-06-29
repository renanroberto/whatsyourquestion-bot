{-# LANGUAGE FlexibleInstances #-}

module Env where

import qualified System.Environment as SysEnv

import           Tracer


class Monad m => MonadEnv m where
  getEnv :: String -> m String
  lookupEnv :: String -> m (Maybe String)

instance MonadEnv IO where
  getEnv = SysEnv.getEnv
  lookupEnv = SysEnv.lookupEnv

instance MonadEnv Tracer where
  getEnv key =
    trace "Env" ("Get environment variable " <> key) >> (pure . tag) key

  lookupEnv key =
    trace "Env" ("Maybe get environment variable " <> key)
    >> (pure . Just . tag) key


tag :: String -> String
tag str = "<" <> str <> ">"

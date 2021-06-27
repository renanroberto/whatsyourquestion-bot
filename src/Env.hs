{-# LANGUAGE FlexibleInstances #-}

module Env where

import qualified System.Environment as SysEnv

import           Logger


class Monad m => MonadEnv m where
  getEnv :: String -> m String
  lookupEnv :: String -> m (Maybe String)

instance MonadEnv IO where
  getEnv = SysEnv.getEnv
  lookupEnv = SysEnv.lookupEnv

instance MonadEnv (Logger [String]) where
  getEnv key =
    logger "Env" ("Get environment variable " <> key) >> (pure . tag) key

  lookupEnv key =
    logger "Env" ("Maybe get environment variable " <> key)
    >> (pure . Just . tag) key


tag :: String -> String
tag str = "<" <> str <> ">"

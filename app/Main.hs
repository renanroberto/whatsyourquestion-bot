{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators #-}

module Main where

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Control.Monad.IO.Class (liftIO)
import Servant
import Network.Wai.Handler.Warp (run)

import BotCore (bot, Update)


type RootAPI = Get '[PlainText] String

type BotAPI =
  "bot"
    :> ReqBody '[JSON] Update
    :> Capture "token" String
    :> Post '[JSON] NoContent

type API = RootAPI
           :<|> BotAPI


getPort :: IO Int
getPort = do
  env <- lookupEnv "PORT"
  return $ fromMaybe 8080 (env >>= readMaybe)

getToken :: IO String
getToken = do
  env <- lookupEnv "TOKEN"
  return $ fromMaybe "" env


rootHandler :: Handler String
rootHandler = return "online"

botHandler :: Update -> String -> Handler NoContent
botHandler update token = do
  envToken <- liftIO getToken
  if envToken == token
    then liftIO (bot update) >> return NoContent
    else return NoContent


server :: Server API
server = rootHandler
         :<|> botHandler

api :: Proxy API
api = Proxy

app :: Application
app = serve api server


main :: IO ()
main = do
  port <- getPort
  putStrLn $ "Server is running on port " ++ (show port)
  run port app

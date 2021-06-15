{-# LANGUAGE DataKinds, TypeOperators #-}
module Main where

import GHC.Conc
import System.Environment (lookupEnv, getEnv)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (run)
import Servant

import TelegramTypes (Update, Recent, Token)
import BotCore


getPort :: IO Int
getPort = do
  env <- lookupEnv "PORT"
  let port = env >>= readMaybe
  pure (fromMaybe 8080 port)

getToken :: IO Token
getToken = getEnv "TOKEN"


type State  = TVar Recent

initialState :: STM State
initialState = newTVar Map.empty


type RootAPI = Get '[PlainText] String

rootAPI :: Server RootAPI
rootAPI = pure "ok"


type BotAPI =
  "bot"
  :> ReqBody '[JSON] Update
  :> Capture "token" Token
  :> Post '[JSON] NoContent

protectedBotAPI :: State -> Server BotAPI
protectedBotAPI state update token = liftIO $ do
  token' <- getToken
  if token == token'
    then botAPI state update
    else pure NoContent

botAPI :: State -> Update -> IO NoContent
botAPI state update = do
  recent <- atomically (readTVar state)
  bot recent update

  atomically $ do
    recent' <- readTVar state
    let updatedRecent = updateRecent update recent'
    writeTVar state updatedRecent

  pure NoContent


type API = RootAPI :<|> BotAPI

api :: Proxy API
api = Proxy

app :: State -> Application
app state = serve api $
  rootAPI :<|> protectedBotAPI state


main :: IO ()
main = do
  port <- getPort
  putStrLn $ "Server is running on port " <> show port
  state <- atomically initialState
  run port (app state)

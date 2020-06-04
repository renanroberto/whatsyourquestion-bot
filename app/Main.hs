{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators #-}

module Main where

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Lazy as Map
import System.Environment (lookupEnv)
import Control.Monad.Trans.State.Lazy hiding (State, state)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Network.Wai.Handler.Warp (run)
import Servant

import BotCore (bot, updateState, Update)


type RootAPI = Get '[PlainText] String

type BotAPI =
  "bot"
    :> ReqBody '[JSON] Update
    :> Capture "token" String
    :> Post '[JSON] NoContent

type BookAPI =
  "book"
    :> Capture "book" String
    :> Get '[JSON] [String]

type API = RootAPI
           :<|> BotAPI
           :<|> BookAPI


data State = State
  { recentUpdates :: TVar (Map.Map Int Update)
  }

type AppM = StateT State Handler


getPort :: IO Int
getPort = do
  env <- lookupEnv "PORT"
  return $ fromMaybe 8080 (env >>= readMaybe)

getToken :: IO String
getToken = do
  env <- lookupEnv "TOKEN"
  return $ fromMaybe "" env


rootHandler :: AppM String
rootHandler = return "online"

botHandler :: Update -> String -> AppM NoContent
botHandler update token = do
  envToken <- liftIO getToken
  if envToken /= token
    then return NoContent
    else do
      state <- get
      let tupdates = recentUpdates state
      updates <- liftIO . atomically . readTVar $ tupdates
      liftIO (bot updates update)
      liftIO . atomically $
        readTVar tupdates
        >>= (writeTVar tupdates . updateState update)
      return NoContent

bookHandler :: String -> AppM [String]
bookHandler book = do
  return ["hey", "there", book]


server :: ServerT API AppM
server = rootHandler
         :<|> botHandler
         :<|> bookHandler

api :: Proxy API
api = Proxy

nt :: State -> AppM a -> Handler a
nt s x = evalStateT x s

app :: State -> Application
app state = serve api $ hoistServer api (nt state) server


main :: IO ()
main = do
  port <- getPort
  putStrLn $ "Server is running on port " ++ (show port)
  initialState <- (atomically . newTVar) Map.empty
  run port $ app (State initialState)

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


module Main where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (ToJSON, toJSON)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe)
import           GHC.Conc
import           Network.HTTP.Types         (status401)
import           Network.Wai.Handler.Warp   (run)
import           Servant
import           Servant.Checked.Exceptions
import           System.Environment         (getEnv, lookupEnv)
import           Text.Read                  (readMaybe)

import           BotCore
import           TelegramTypes              (Recent, Token, Update)


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


data InvalidToken = InvalidToken

instance ToJSON InvalidToken where
  toJSON _ = toJSON "invalid token"

instance ErrStatus InvalidToken where
  toErrStatus _ = status401

type BotAPI =
  "bot"
  :> ReqBody '[JSON] Update
  :> Capture "token" Token
  :> Throws InvalidToken
  :> Post '[JSON] String

protectedBotAPI :: State -> Server BotAPI
protectedBotAPI state update token = liftIO $ do
  token' <- getToken
  if token == token'
    then botAPI state update *> pureSuccEnvelope ""
    else pureErrEnvelope InvalidToken

botAPI :: State -> Update -> IO ()
botAPI state update = do
  recent <- atomically (readTVar state)
  bot recent update

  atomically $ do
    recent' <- readTVar state
    let updatedRecent = updateRecent update recent'
    writeTVar state updatedRecent


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

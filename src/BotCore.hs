{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module BotCore (bot, Update) where

import GHC.Generics
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Control.Monad ((>=>))
import Data.Aeson
import Network.Wreq


data Chat = Chat
  { chat_id :: Int
  , chat_type :: String
  } deriving (Generic, Show)

instance ToJSON Chat where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = dropPrefix "chat_" }

instance FromJSON Chat where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = dropPrefix "chat_" }


data User = User
  { user_id :: Int
  , user_first_name :: String
  } deriving (Generic, Show)

instance ToJSON User where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = dropPrefix "user_" }

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = dropPrefix "user_" }


data Message = Message
  { message_message_id :: Int
  , message_from :: Maybe User
  , message_date :: Int
  , message_chat :: Chat
  , message_text :: Maybe String
  } deriving (Generic, Show)

instance ToJSON Message where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = dropPrefix "message_" }

instance FromJSON Message where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = dropPrefix "message_" }


data Update = Update
  { update_update_id :: Int
  , update_message :: Maybe Message
  } deriving (Generic, Show)

instance ToJSON Update where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = dropPrefix "update_" }

instance FromJSON Update where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = dropPrefix "update_" }


data Telegram = Telegram
  { ok :: Bool
  , result :: [Update]
  } deriving (Generic, ToJSON, FromJSON)


data SendMessage = SendMessage
  { sendmessage_chat_id :: Int
  , sendmessage_reply_to_message_id :: Int
  , sendmessage_text :: String
  , sendmessage_parse_mode :: String
  } deriving (Generic)

instance ToJSON SendMessage where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = dropPrefix "sendmessage_" }

instance FromJSON SendMessage where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = dropPrefix "sendmessage_" }


dropPrefix :: String -> String -> String
dropPrefix prefix str = drop (length prefix) str


getToken :: IO String
getToken = do
  env <- lookupEnv "TOKEN"
  return $ fromMaybe "" env
  

api :: String -> String -> String
api token method = "https://api.telegram.org/bot" ++ token ++ method


{-- Not using: getting one update per time
getQuestions :: [Message] -> [Message]
getQuestions [] = []
getQuestions (msg:messages) =
  let text = fromMaybe "" (message_text msg) in
    if isQuestion text
       then msg : (getQuestions messages)
       else getQuestions messages
--}

getQuestion :: Message -> Maybe Message
getQuestion msg =
  let text = fromMaybe "" (message_text msg) in
    if isQuestion text
       then Just msg
       else Nothing
    
isQuestion :: String -> Bool
isQuestion [] = False
isQuestion str = all (== '?') str
 

replyToQuestion :: Message -> IO ()
replyToQuestion question =
  let
    chat = (chat_id . message_chat) question
    reply = message_message_id question
    parseMode = "MarkdownV2"
    inquirer = user_first_name <$> (message_from question)
  in
    sendMessage $ SendMessage chat reply (getText inquirer) parseMode
  where
    getText :: Maybe String -> String
    getText Nothing = "Qual é a sua dúvida?"
    getText (Just name) = "Qual é a sua dúvida, " ++ safeName name ++ "?"


safeName :: String -> String
safeName name = "`" ++ name ++ "`"
 
{-- Not using: Using webhooks
getUpdates :: IO [Update]
getUpdates = do
  token <- getToken
  res <- asJSON =<< get (api token "/getUpdates")
  return $ result (res ^. responseBody)
--}

sendMessage :: SendMessage -> IO ()
sendMessage message = do
  token <- getToken
  _ <- post (api token "/sendMessage") (toJSON message)
  return ()


bot :: Update -> IO ()
bot update = 
  case (update_message >=> getQuestion) update of
    Just question -> replyToQuestion question
    Nothing -> return ()

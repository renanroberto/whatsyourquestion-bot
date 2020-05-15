{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module BotCore (bot, Update) where

import GHC.Generics
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Control.Monad ((<=<))
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
  , sendmessage_disable_web_page_preview :: Bool
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


getQuestion :: Message -> Maybe (Message, String)
getQuestion msg =
  let text = fromMaybe "" (message_text msg) in
    if isQuestion text
       then Just (msg, take 10 text)
       else Nothing
    
isQuestion :: String -> Bool
isQuestion ('?':str) = all (`elem` ['?', '!']) str
isQuestion _ = False


replyToQuestion :: (Message, String) -> IO ()
replyToQuestion (question, marks) =
  let
    chat = (chat_id . message_chat) question
    reply = message_message_id question
    inquirer = user_first_name <$> (message_from question)
    parseMode = ""
    disablePreview = True
  in
    sendMessage $
      SendMessage
        { sendmessage_chat_id = chat
        , sendmessage_reply_to_message_id = reply
        , sendmessage_text = getText inquirer marks
        , sendmessage_parse_mode = parseMode
        , sendmessage_disable_web_page_preview = disablePreview
        }

getText :: Maybe String -> String -> String
getText Nothing marks = "Qual é a sua dúvida" ++ marks
getText (Just name) marks =
  "Qual é a sua dúvida, " ++ safeName name ++ marks

safeName :: String -> String
safeName = filter (/= '@')
 

sendMessage :: SendMessage -> IO ()
sendMessage message = do
  token <- getToken
  _ <- post (api token "/sendMessage") (toJSON message)
  return ()


bot :: Update -> IO ()
bot update = 
  case (getQuestion <=< update_message) update of
    Just question -> replyToQuestion question
    Nothing -> return ()

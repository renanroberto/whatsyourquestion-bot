{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module BotCore (bot, Update) where

import GHC.Generics
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Network.Wreq (post)
import Data.Aeson
  ( ToJSON
  , FromJSON
  , toJSON
  , parseJSON
  , fieldLabelModifier
  , genericParseJSON
  , defaultOptions
  , genericToJSON
  )

import SafeName

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

(<?>) :: (b -> Bool) -> (a -> Maybe b) -> (a -> Bool)
(<?>) f g x = fromMaybe False $ fmap f (g x)


getToken :: IO String
getToken = do
  env <- lookupEnv "TOKEN"
  return $ fromMaybe "" env
  

api :: String -> String -> String
api token method = "https://api.telegram.org/bot" ++ token ++ method


sendMessage :: SendMessage -> IO ()
sendMessage message = do
  token <- getToken
  _ <- post (api token "/sendMessage") (toJSON message)
  return ()


needAnswer :: Message -> Bool
needAnswer = isQuestion <?> message_text

isQuestion :: String -> Bool
isQuestion ('?':str) = all (`elem` ['?', '!']) str
isQuestion "¿" = True
isQuestion "‽" = True
isQuestion _ = False


answerQuestion :: Message -> SendMessage
answerQuestion message =
  let
    chat = (chat_id . message_chat) message
    reply = message_message_id message
    text = fromMaybe "" $ message_text message
    inquirer = user_first_name <$> (message_from message)
    disablePreview = True
  in
    SendMessage
      { sendmessage_chat_id = chat
      , sendmessage_reply_to_message_id = reply
      , sendmessage_text = getText text (safeName <$> inquirer)
      , sendmessage_disable_web_page_preview = disablePreview
      }

getText :: String -> Maybe SafeName -> String
getText "¿" _ = "¿ɐpᴉʌn̗p ɐns ɐ ǝ̗ ʅɐnꝹ" -- Boa noite, Bruno
getText _ Nothing = "Qual é a sua dúvida?"
getText marks (Just name) =
  "Qual é a sua dúvida, " ++ unsafeName name ++ (take 10 marks)

bot :: Update -> IO ()
bot update =
  case update_message update of
    Nothing -> return ()
    Just message -> if needAnswer message
      then (sendMessage . answerQuestion) message
      else return ()

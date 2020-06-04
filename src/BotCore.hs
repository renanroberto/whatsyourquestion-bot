{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module BotCore (bot, updateState, Update) where

import GHC.Generics
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Lazy as Map
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

import SafeName (safeName)
import Answer (isQuestion, getAnswer)


data Chat = Chat
  { chat_id :: Int
  , chat_type :: String
  } deriving (Generic, Eq, Show)

instance ToJSON Chat where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = dropPrefix "chat_" }

instance FromJSON Chat where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = dropPrefix "chat_" }


data User = User
  { user_id :: Int
  , user_first_name :: String
  } deriving (Generic, Eq, Show)

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
  } deriving (Generic, Eq, Show)

instance ToJSON Message where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = dropPrefix "message_" }

instance FromJSON Message where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = dropPrefix "message_" }


data Update = Update
  { update_update_id :: Int
  , update_message :: Maybe Message
  } deriving (Generic, Eq, Show)

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


type State = Map.Map Int Update


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


updateState :: Update -> State -> State
updateState update state =
  let
    chat = fromMaybe 0 $ chat_id . message_chat <$> update_message update
  in
    Map.insert chat update state


needAnswer :: Message -> Bool
needAnswer = isQuestion <?> message_text


hasRecentMessage :: State -> Update -> Bool
hasRecentMessage state update =
  let
    chat = fromMaybe 0 $ chat_id . message_chat <$> update_message update
    author = message_from <$> update_message update
  in
    (sameAuthor update <?> Map.lookup chat) state

sameAuthor :: Update -> Update -> Bool
sameAuthor x y = getAuthor x == getAuthor y
  where getAuthor u = update_message u >>= message_from


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
      , sendmessage_text = getAnswer text (safeName <$> inquirer)
      , sendmessage_disable_web_page_preview = disablePreview
      }


bot :: State -> Update -> IO ()
bot state update =
  case update_message update of
    Nothing -> return ()
    Just message ->
      if needAnswer message && not (hasRecentMessage state update)
        then (sendMessage . answerQuestion) message
        else return ()

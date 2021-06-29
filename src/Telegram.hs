{-# LANGUAGE DuplicateRecordFields #-}

module Telegram where

import           Control.Lens
import           Data.Maybe    (fromMaybe)

import           Env
import           HTTP
import           TelegramTypes


answerQuestion :: Update -> String -> SendMessage
answerQuestion update answer = SendMessage
  { chat_id = fromMaybe 0 $
    update ^? _message . _Just . _chat . _chat_id
  , reply_to_message_id = fromMaybe 0 $
    update ^? _message . _Just . _message_id
  , text = answer
  , disable_web_page_preview = True
  }

api :: String -> String -> String
api token method = "https://api.telegram.org/bot" <> token <> method

sendMessage :: (MonadEnv m, MonadHTTP m) => SendMessage -> m ()
sendMessage msg = do
  token <- getEnv "TOKEN"
  _ <- post (api token "/sendMessage") msg
  pure ()

sendAnswer :: (MonadEnv m, MonadHTTP m) => Update -> String -> m ()
sendAnswer update msg =
  sendMessage . answerQuestion update $ msg

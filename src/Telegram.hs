{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}

module Telegram where

import           Control.Lens
import           Data.Aeson         (toJSON)
import           Data.Maybe         (fromMaybe)
import           Network.Wreq       (post)
import           System.Environment (getEnv)

import           Logger
import           TelegramTypes


class Monad m => MonadTelegram m where
  sendAnswer :: Update -> String -> m ()

instance MonadTelegram IO where
  sendAnswer update msg =
    sendMessage . answerQuestion update $ msg

instance MonadTelegram (Logger [String]) where
  sendAnswer _update msg = logger "Telegram" msg


api :: String -> String -> String
api token method = "https://api.telegram.org/bot" <> token <> method

answerQuestion :: Update -> String -> SendMessage
answerQuestion update answer = SendMessage
  { chat_id = fromMaybe 0 $
    update ^? _message . _Just . _chat . _chat_id
  , reply_to_message_id = fromMaybe 0 $
    update ^? _message . _Just . _message_id
  , text = answer
  , disable_web_page_preview = True
  }

sendMessage :: SendMessage -> IO ()
sendMessage msg = do
  token <- getEnv "TOKEN"
  _ <- post (api token "/sendMessage") (toJSON msg)
  pure ()

{-# LANGUAGE DuplicateRecordFields, RankNTypes, FlexibleInstances #-}

module BotCore (bot, core, updateRecent, shouldAnswer) where

import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Control.Lens

import Logger
import Question
import SafeName
import Flow
import Telegram
import TelegramTypes


{--
                          sim                       sim
mensagem -> é uma dúvida? -> responde outra pessoa? -> questiona
                | não                  | não
                v                      v                sim
              noop     última mensagem do grupo é dele? -> noop
                                       | não
                                       v
                                   questiona
--}

isReply :: Update -> Bool
isReply update = fromMaybe False $ do
  msg <- update ^? _message . _Just
  let getUserId = _from ._Just . _user_id
  user <- msg ^? getUserId
  replyUser <- msg ^? _reply_to_message . _Just . getUserId
  pure $ user /= replyUser


sameAuthor :: Message -> Message -> Bool
sameAuthor x y = fromMaybe False $ do
  xId <- x ^? _from . _Just . _user_id
  yId <- y ^? _from . _Just . _user_id
  pure (xId == yId)

isLastMessage :: Recent -> Update -> Bool
isLastMessage recent update = fromMaybe False $ do
  currentMessage <- update ^? _message . _Just
  let chatId = currentMessage ^. _chat . _chat_id
  savedMessage <- Map.lookup chatId recent ^? _Just . _message . _Just
  pure $ sameAuthor currentMessage savedMessage


shouldAnswer :: Recent -> Update -> Bool
shouldAnswer recent update = flow $ do
  (not . isQuestion) msg              ?> False
  isReply update                      ?> True
  (not . isLastMessage recent) update ?> True
  where msg = fromMaybe "" $
          update ^? _message . _Just . _message_text . _Just


getMessageQuestion :: Update -> Question
getMessageQuestion update = fromMaybe defaultQuestion $ do
  msg <- update ^? _message . _Just . _message_text . _Just
  question msg

getUserName :: Update -> Maybe SafeName
getUserName update = do
  name <- update ^? _message . _Just . _from . _Just . _first_name
  safeName name


updateRecent :: Update -> Recent -> Recent
updateRecent update recent =
  let chatId = fromMaybe 0 $ update ^? _message . _Just . _chat . _chat_id
  in Map.insert chatId update recent


class Monad m => MonadTelegram m where
  sendAnswer :: Update -> String -> m ()

instance MonadTelegram IO where
  sendAnswer update msg =
    sendMessage . answerQuestion update $ msg

instance MonadTelegram (Logger [String]) where
  sendAnswer _update msg = logger "Telegram" msg


core :: MonadTelegram m => Recent -> Update -> m ()
core recent update
  | shouldAnswer recent update = do
      let messageQuestion = getMessageQuestion update
      let name = getUserName update
      let answer = chooseAnswer messageQuestion name
      sendAnswer update answer

  | otherwise = pure ()


bot :: Recent -> Update -> IO ()
bot = core 

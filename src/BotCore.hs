module BotCore (bot, core, updateRecent, shouldAnswer) where

import           Control.Lens
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)

import           Env
import           Flow            (flow, (?>))
import           HTTP
import           Question
import           SafeName        (SafeName, safeName)
import           Telegram        (sendAnswer)
import           TelegramTypes

{--
                          yes                       yes
message -> is a question? -> anwswer another person? -> answer
                | no                   | no
                v                      v                    yes
              noop     it was the last user to send message? -> noop
                                       | no
                                       v
                                     answer
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


core :: (MonadEnv m, MonadHTTP m) => Recent -> Update -> m ()
core recent update
  | shouldAnswer recent update = do
      let messageQuestion = getMessageQuestion update
      let name = getUserName update
      let answer = chooseAnswer messageQuestion name
      sendAnswer update answer

  | otherwise = pure ()

bot :: Recent -> Update -> IO ()
bot = core

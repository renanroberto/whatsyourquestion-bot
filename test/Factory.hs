{-# LANGUAGE DuplicateRecordFields #-}
module Factory where

import TelegramTypes

replyFactory :: Int -> String -> Maybe Message
replyFactory userId msg = Just $ Message
  { message_id = 2
  , from = Just $ User
    { id = userId
    , first_name = "other user"
    }
  , reply_to_message = Nothing
  , date = 0
  , chat = Chat { id = 1 }
  , text = Just msg
  }

updateFactory :: Maybe Message -> String -> Update
updateFactory reply mark = Update
  { update_id = 1
  , message = Just $ Message
    { message_id = 1
    , from = Just $ User
      { id = 1
      , first_name = "some user"
      }
    , reply_to_message = reply
    , date = 0
    , chat = Chat { id = 1 }
    , text = Just mark
    }
  }

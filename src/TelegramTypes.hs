{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass, DuplicateRecordFields #-}

module TelegramTypes where

import Prelude hiding (id)
import GHC.Generics
import Data.Aeson
import Control.Lens
import qualified Data.Map.Strict as Map


data Chat = Chat
  { id :: Int
  } deriving (Show, Generic, ToJSON, FromJSON)

makeLensesFor
  [ ("id", "_chat_id")
  ] ''Chat


data User = User
  { id         :: Int
  , first_name :: String
  } deriving (Show, Generic, ToJSON, FromJSON)

makeLensesFor
  [ ("id", "_user_id")
  , ("first_name", "_first_name")
  ] ''User


data Message = Message
  { message_id       :: Int
  , from             :: Maybe User
  , reply_to_message :: Maybe Message
  , date             :: Int
  , chat             :: Chat
  , text             :: Maybe String
  } deriving (Show, Generic, ToJSON, FromJSON)


makeLensesFor
  [ ("message_id", "_message_id")
  , ("text", "_message_text")
  , ("from", "_from")
  , ("chat", "_chat")
  , ("reply_to_message", "_reply_to_message")
  ] ''Message


data Update = Update
  { update_id :: Int
  , message   :: Maybe Message
  } deriving (Show, Generic, ToJSON, FromJSON)

makeLensesFor
  [ ("update_id", "_update_id")
  , ("message", "_message")
  ] ''Update


data SendMessage = SendMessage
  { chat_id                  :: Int
  , reply_to_message_id      :: Int
  , text                     :: String
  , disable_web_page_preview :: Bool
  } deriving (Show, Generic, ToJSON, FromJSON)

makeLensesFor
  [ ("text", "_send_text")
  ] ''SendMessage


type Recent = Map.Map Int Update

type Token  = String

{--
example :: Update
example = Update
  { update_id = 1
  , message = Just $ Message
    { message_id = 2
    , from = Just $ User
      { id = 3
      , first_name = "Renan"
      }
    , reply_to_message = Just $ Message
      { message_id = 15
      , from = Nothing
      , reply_to_message = Nothing
      , date = 0
      , chat = Chat { id = 5 }
      , text = Nothing
      }
    , date = 0
    , chat = Chat { id = 4 }
    , text = Just "Hello World"
    }
  }

example' :: Update
example' = Update
  { update_id = 1
  , message = Just $ Message
    { message_id = 2
    , from = Just $ User
      { id = 3
      , first_name = "Renan"
      }
    , reply_to_message = Nothing
    , date = 0
    , chat = Chat { id = 4 }
    , text = Just "Hello World"
    }
  }
--}

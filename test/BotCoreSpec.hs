{-# LANGUAGE DuplicateRecordFields #-}
module BotCoreSpec where

import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Function ((&))

import BotCore
import TelegramTypes
import Logger

import Factory
import Test.Hspec


spec :: Spec
spec = do
  describe "shouldAnswer" $ do
    it "should answer question" $ do
      "???"
      & updateFactory Nothing
      & shouldAnswer Map.empty
      & shouldBe True

    it "should not answer afirmation" $ do
      "you're right!"
      & updateFactory Nothing
      & shouldAnswer Map.empty
      & shouldBe False

    it "should answer a reply" $ do
      let reply = replyFactory 2 "foo"
      let update = updateFactory reply "?!"
      let recent = Map.singleton 1 update
      let result = shouldAnswer recent update

      result `shouldBe` True

    it "should not answer self reply" $ do
      let reply = replyFactory 1 "foo"
      let update = updateFactory reply "?!"
      let recent = Map.singleton 1 update
      let result = shouldAnswer recent update

      result `shouldBe` False

    it "should not answer when it was the last message" $ do
      let update = updateFactory Nothing "?!"
      let recent = Map.singleton 1 update
      
      shouldAnswer recent update `shouldBe` False

    it "should answer when it was last message of another chat" $ do
      let update = updateFactory Nothing "?!"
      let recent = Map.singleton 2 update

      shouldAnswer recent update `shouldBe` True

  describe "updateRecent" $ do
    it "should update recent message of the chat" $ do
      let update = updateFactory Nothing "ok"
      let recent = Map.singleton 2 update
      let recent' = updateRecent update recent

      Map.size recent' `shouldBe` 2
      Map.lookup 1 recent' `shouldSatisfy` isJust
      Map.lookup 0 recent' `shouldSatisfy` isNothing

  describe "core" $ do
    it "should answer the question" $ do
      "?!?!"
      & updateFactory Nothing
      & core Map.empty
      & runLogger
      & snd
      & unlines
      & shouldBe "[Telegram]\tQual é a sua dúvida, some user?!?!\n"

    it "should not answer the message" $ do
      "yup"
      & updateFactory Nothing
      & core Map.empty
      & runLogger
      & snd
      & unlines
      & shouldBe ""

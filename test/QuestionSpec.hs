module QuestionSpec where


import Data.Maybe (fromJust, isJust, isNothing)

import Question
import SafeName (safeName)

import Test.Hspec


spec :: Spec
spec = do
  describe "isQuestion" $ do
    it "returns true to question marks" $ do
      isQuestion "?" `shouldBe` True
      isQuestion "???" `shouldBe` True

    it "returns true to question marks with dead marks" $ do
      isQuestion "?!" `shouldBe` True
      isQuestion "(?!)" `shouldBe` True
      isQuestion ".?" `shouldBe` True
      isQuestion "\\?" `shouldBe` True

    it "returns true to special question marks" $ do
      isQuestion "‽" `shouldBe` True
      isQuestion "⁇" `shouldBe` True

    it "returns false to invalid question" $ do
      isQuestion "what do you mean?" `shouldBe` False
      isQuestion "wtf?" `shouldBe` False
      isQuestion "hm?" `shouldBe` False

    it "returns false to an afirmation" $ do
      isQuestion "you're right" `shouldBe` False
      isQuestion "yep" `shouldBe` False
      isQuestion "!" `shouldBe` False

    it "returns false to empty string" $ do
      isQuestion "" `shouldBe` False


  describe "question" $ do
    it "returns Just to question marks" $ do
      question "?" `shouldSatisfy` isJust
      question "???" `shouldSatisfy` isJust

    it "returns Nothing to invalid question" $ do
      question "what do you mean?" `shouldSatisfy` isNothing
      question "wtf?" `shouldSatisfy` isNothing
      question "hm?" `shouldSatisfy` isNothing


  describe "chooseAnswer" $ do
    it "returns the correct answer knowing name" $ do
      let qst = (fromJust . question) "?!"
      let name = safeName "Renan"

      chooseAnswer qst name == "Qual é a sua dúvida, Renan?!" 

    it "returns the correct answer not knowing name" $ do
      let qst = (fromJust . question) "?!"
      let name = Nothing

      chooseAnswer qst name == "Qual é a sua dúvida?!" 

    it "do not flood marks" $ do
      let qst = (fromJust . question . replicate 15) '?'
      let name = Nothing

      chooseAnswer qst Nothing ==
        "Qual é a sua dúvida" <> (replicate 10 '?')

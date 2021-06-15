module SafeNameSpec where

import Data.Maybe (fromJust, isJust, isNothing)
import SafeName
import Test.Hspec

spec :: Spec
spec = do
  describe "safeName" $ do
    it "returns (Just name) on valid name" $ do
      let name = safeName "Renan"

      name `shouldSatisfy` isJust
      (getName . fromJust) name `shouldBe` "Renan"
      
    it "returns Nothing on invalid name" $ do
      safeName "@renan_r" `shouldSatisfy` isNothing

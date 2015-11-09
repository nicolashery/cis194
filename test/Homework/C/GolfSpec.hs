{-# OPTIONS_GHC -Wall #-}
module Homework.C.GolfSpec where

import Homework.C.Golf
import Test.Hspec

spec :: Spec
spec = describe "Homework.C.Golf" $ do

  describe "skips" $ do
    it "works on an empty list" $ do
      skips [] `shouldBe` ([] :: [[Integer]])

    it "works on a list with one integer" $ do
      skips [1] `shouldBe` ([[1]] :: [[Integer]])

    it "works on a list of booleans" $ do
      skips [True,False] `shouldBe` [[True,False], [False]]

    it "works on a string" $ do
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]

    it "works on another string" $ do
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]

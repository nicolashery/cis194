{-# OPTIONS_GHC -Wall #-}
module Homework.C.GolfSpec where

import Homework.C.Golf
import Test.Hspec

histogram1 :: String
histogram1 = unlines
  [ " *        "
  , " *        "
  , " *   *    "
  , "=========="
  , "0123456789" ]

histogram2 :: String
histogram2 = unlines
  [ "    *     "
  , "    *     "
  , "    * *   "
  , " ******  *"
  , "=========="
  , "0123456789" ]

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

  describe "localMaxima" $ do
    it "returns an empty list when there are no local maxima" $ do
      localMaxima [1,2,3,4,5] `shouldBe` []

    it "returns a list with the local maxima it found" $ do
      localMaxima [2,3,4,1,5] `shouldBe` [4]

    it "returns multiple local maxima if they exist" $ do
      localMaxima [2,9,5,6,1] `shouldBe` [9,6]

  describe "histogram" $ do
    it "works for one input" $ do
      histogram [1,1,1,5] `shouldBe` histogram1

    it "works for another input" $ do
      histogram [1,4,5,4,6,6,3,4,2,4,9] `shouldBe` histogram2

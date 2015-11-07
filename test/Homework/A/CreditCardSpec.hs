{-# OPTIONS_GHC -Wall #-}
module Homework.A.CreditCardSpec where

import Homework.A.CreditCard
import Test.Hspec

spec :: Spec
spec = describe "Homework.A.CreditCard" $ do

  describe "toDigitsRev" $ do
    it "converts positive Integers to a reversed list of digits" $ do
      toDigitsRev 1234 `shouldBe` [4,3,2,1]

    it "returns the empty list for 0 input" $ do
      toDigitsRev 0 `shouldBe` []

    it "returns the empty list for negative inputs" $ do
      toDigitsRev (-17) `shouldBe` []

  describe "toDigits" $ do
    it "converts positive Integers to a list of digits" $ do
      toDigits 1234 `shouldBe` [1,2,3,4]

    it "returns the empty list for 0 input" $ do
      toDigits 0 `shouldBe` []

    it "returns the empty list for negative inputs" $ do
      toDigits (-17) `shouldBe` []

  describe "doubleEveryOther" $ do
    it "doubles every other Integer from the right for a list of even length" $ do
      doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]

    it "doubles every other Integer from the right for a list of odd length" $ do
      doubleEveryOther  [1,2,3] `shouldBe` [1,4,3]

  describe "sumDigits" $ do
    it "sums all digits in a list with one or two-digit numbers" $ do
      sumDigits [16,7,12,5] `shouldBe` 22

  describe "validate" $ do
    it "returns True for valid credit card numbers" $ do
      validate 4012888888881881 `shouldBe` True

    it "returns False for invalid credit card numbers" $ do
      validate 4012888888881882 `shouldBe` False

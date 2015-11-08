{-# OPTIONS_GHC -Wall #-}
module Homework.A.HanoiSpec where

import Homework.A.Hanoi
import Test.Hspec

a = "a"
b = "b"
c = "c"

spec :: Spec
spec = describe "Homework.A.Hanoi" $ do

  describe "hanoi" $ do
    it "solves the problem for 1 disc" $ do
      hanoi 1 a b c `shouldBe` [(a,b)]

    it "solves the problem for 2 discs" $ do
      hanoi 2 a b c `shouldBe` [(a,c), (a,b), (c,b)]

    it "solves the problem for 3 discs" $ do
      hanoi 3 a b c `shouldBe` [(a,b), (a,c), (b,c), (a,b), (c,a), (c,b), (a,b)]

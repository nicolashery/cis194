{-# OPTIONS_GHC -Wall #-}
module Homework.B.LogAnalysisSpec where

import Homework.B.Log
import Homework.B.LogAnalysis
import Test.Hspec

tree :: MessageTree
tree = Node
  (Node Leaf (LogMessage Info 1 "1") Leaf)
  (LogMessage Info 3 "3")
  (Node Leaf (LogMessage Info 5 "5") Leaf)

spec :: Spec
spec = describe "Homework.B.LogAnalysis" $ do

  describe "parseMessage" $ do
    it "parses an info message" $ do
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"

    it "parses a warning message" $ do
      parseMessage "W 32 careful careful" `shouldBe` LogMessage Warning 32 "careful careful"

    it "parses an error message" $ do
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"

    it "parses an unknown message" $ do
      parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"

  describe "insert" $ do
    it "returns tree unchanged given an unknown log message" $ do
      insert (Unknown "") tree `shouldBe` tree

    it "inserts log message at the correct place in the tree" $ do
      insert (LogMessage Info 2 "2") tree `shouldBe`
        Node
          (Node Leaf (LogMessage Info 1 "1") (Node Leaf (LogMessage Info 2 "2") Leaf))
          (LogMessage Info 3 "3")
          (Node Leaf (LogMessage Info 5 "5") Leaf)

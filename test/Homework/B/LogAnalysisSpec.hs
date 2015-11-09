{-# OPTIONS_GHC -Wall #-}
module Homework.B.LogAnalysisSpec where

import Homework.B.Log
import Homework.B.LogAnalysis
import Test.Hspec

smallMessages :: [LogMessage]
smallMessages =
  [ (LogMessage Info 1 "1")
  , (LogMessage Info 5 "5")
  , (LogMessage Info 3 "3")
  , (Unknown "")
  ]

smallTree :: MessageTree
smallTree = Node
  (Node Leaf (LogMessage Info 1 "1") Leaf)
  (LogMessage Info 3 "3")
  (Node Leaf (LogMessage Info 5 "5") Leaf)

messages :: [LogMessage]
messages =
  [ (LogMessage Info 2 "2")
  , (LogMessage Info 1 "1")
  , (LogMessage Info 5 "5")
  , (LogMessage Info 3 "3")
  , (Unknown "")
  ]

tree :: MessageTree
tree = Node
  (Node Leaf (LogMessage Info 1 "1") (Node Leaf (LogMessage Info 2 "2") Leaf))
  (LogMessage Info 3 "3")
  (Node Leaf (LogMessage Info 5 "5") Leaf)

sortedMessages :: [LogMessage]
sortedMessages =
  [ (LogMessage Info 1 "1")
  , (LogMessage Info 2 "2")
  , (LogMessage Info 3 "3")
  , (LogMessage Info 5 "5")
  ]

postmortemMessages :: [LogMessage]
postmortemMessages =
  [ (LogMessage Info 1 "I1")
  , (LogMessage (Error 40) 2 "E2")
  , (LogMessage (Error 50) 4 "E4")
  , (LogMessage (Error 51) 3 "E3")
  ]

postmortemResult :: [String]
postmortemResult = ["E3", "E4"]

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
      insert (LogMessage Info 2 "2") smallTree `shouldBe` tree

  describe "build" $ do
    it "builds a complete tree from a list of messages" $ do
      build messages `shouldBe` tree

  describe "inOrder" $ do
    it "produces a list of messages sorted by timestamp from smallest to biggest" $ do
      inOrder tree `shouldBe` sortedMessages

  describe "whatWentWrong" $ do
    it " produces a list of the text from relevant messages sorted by timestamp" $ do
      whatWentWrong postmortemMessages `shouldBe` postmortemResult

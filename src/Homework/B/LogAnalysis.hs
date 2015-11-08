{-# OPTIONS_GHC -Wall #-}
module Homework.B.LogAnalysis where

import Homework.B.Log

-- Exercise 1

parseTime :: String -> TimeStamp
parseTime = read

parseLevel :: String -> Int
parseLevel = read

parseMessage :: String -> LogMessage
parseMessage message =
  let tokens = words message

  in case tokens of
    ("I":time:rest) ->
      LogMessage Info (parseTime time) (unwords rest)

    ("W":time:rest) ->
      LogMessage Warning (parseTime time) (unwords rest)

    ("E":level:time:rest) ->
      LogMessage (Error (parseLevel level)) (parseTime time) (unwords rest)

    _ ->
      Unknown (unwords tokens)

parse :: String -> [LogMessage]
parse input =
  let messages = lines input
  in map parseMessage messages

-- Exercise 2

getMessageTime :: LogMessage -> TimeStamp
getMessageTime (LogMessage (Error _) time _) = time
getMessageTime (LogMessage _ time _) = time
-- We don't expect an Unknown message, but to make matching exhaustive...
getMessageTime _ = 0

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf     = Node Leaf message Leaf
insert message (Node left nodeMessage right) =
  let messageTime = getMessageTime message
      nodeMessageTime = getMessageTime nodeMessage
  in
    if messageTime < nodeMessageTime
    then Node (insert message left) nodeMessage right
    else Node left nodeMessage (insert message right)

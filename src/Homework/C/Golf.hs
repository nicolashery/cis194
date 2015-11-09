{-# OPTIONS_GHC -Wall #-}
module Homework.C.Golf where

-- Exercise 1 Hopscotch

skips :: [a] -> [[a]]
skips xs =
  let indexed = zip [1..length xs] xs
      build (i, _) = filter (\(j, _) -> j `mod` i == 0) indexed
      extract built = map (\(_, x) -> x) built
  in map (extract . build) indexed

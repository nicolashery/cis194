{-# OPTIONS_GHC -Wall #-}
module Homework.C.Golf where

-- Exercise 1 Hopscotch

skips :: [a] -> [[a]]
skips xs =
  let indexed = zip [1..length xs] xs
      build (i, _) = filter (\(j, _) -> j `mod` i == 0) indexed
      extract built = map snd built
  in map (extract . build) indexed

-- Exercise 2 Local maxima

localMaxima :: [Integer] -> [Integer]
localMaxima xs@(x:y:z:_) =
  (if y > x && y > z then [y] else []) ++ (localMaxima (drop 1 xs))
localMaxima _ = []

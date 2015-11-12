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

-- Exercise 3 Histogram
histogram :: [Integer] -> String
histogram xs =
  let buckets = [0,1,2,3,4,5,6,7,8,9]
      -- List with the number of occurences for each integer (0-9)
      frequences = map (\i -> length (filter ((==) i) xs)) buckets
      -- List of tuples (integer, frequence) representing the histogram
      pairs = zip buckets frequences
      -- "Height" of the histogram is 2 lines (for the integers and "="s)
      -- plus the maximum frequence
      height = 2 + (foldl max 0 frequences)
      -- Start by building a "flipped" version of the histogram, i.e.:
      -- ["0=*  ", "1=**", etc.]
      visualize (i, n) =
        show i ++ "=" ++ replicate n '*' ++ replicate (height - n) ' '
      flipped = map visualize pairs
      -- "Expand" the flipped version to an intermediate representation:
      -- [["0=*  ", "1=**"], ["=*  ", "=**"], ["*  ", "**"], etc.]
      expanded = take height (iterate (map (drop 1)) flipped)
      -- ..so we can take the first item of each sublist and create the lines:
      -- ["01", "==", "**", etc.]
      concatFirsts l = foldl (++) "" (map (take 1) l)
      reversed = map concatFirsts expanded
      -- Which we reverse and join with "\n"
  in unlines (reverse reversed)

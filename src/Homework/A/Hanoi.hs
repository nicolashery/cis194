{-# OPTIONS_GHC -Wall #-}
module Homework.A.Hanoi where

-- Exercise 5

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n source target temp
 | n == 1 = [(source, target)]
 | otherwise =
    -- First, move n-1 discs using "b" as the "temp" and "c" as the "target"
    (hanoi (n - 1) source temp target) ++
    -- Then, move the big disc from "a" ("source") to "b" ("target")
    [(source, target)] ++
    -- Finally, move n-1 discs using "c" as the "source" and "a" as the "temp"
    (hanoi (n - 1) temp target source)

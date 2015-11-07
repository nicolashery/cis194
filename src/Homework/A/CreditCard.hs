{-# OPTIONS_GHC -Wall #-}
module Homework.A.CreditCard where

-- Exercise 1

-- Convert positive Integers to a reversed list of digits
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = (n `mod` 10) : (toDigitsRev (n `div` 10))

-- Convert positive Integers to a list of digits
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- Exercise 2

-- Double every other Integer in a list
doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft []       = []
doubleEveryOtherLeft (x:[])   = [x]
doubleEveryOtherLeft (x:y:zs) = x : (y * 2) : doubleEveryOtherLeft zs

-- Double every other Integer in a list, beginning from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherLeft . reverse

-- Exercise 3

-- Sum all digits in a list with a mix of one-digit and two-digit numbers
sumDigits :: [Integer] -> Integer
sumDigits xs = sum (concatMap toDigits xs)

-- Exercise 4

-- Indicate whether an Integer could be a valid credit card number
validate :: Integer -> Bool
validate number = total `mod` 10 == 0
  where total = (sumDigits . doubleEveryOther . toDigits) number

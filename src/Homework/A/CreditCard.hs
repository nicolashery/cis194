{-# OPTIONS_GHC -Wall #-}
module Homework.A.CreditCard where

-- Exercise 1

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = (toDigits (n `div` 10)) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

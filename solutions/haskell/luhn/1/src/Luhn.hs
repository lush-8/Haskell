module Luhn (isValid) where

import Data.Char (digitToInt, isDigit, isSpace)

isValid :: String -> Bool
isValid n
  | length clean <= 1 = False
  | not (all isDigit clean) = False
  | otherwise = sum transformed `mod` 10 == 0
  where
    clean = filter (not . isSpace) n

    digits = map digitToInt clean
    reversedDigits = reverse digits

    transformed = doubleEveryOther reversedDigits

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs) = x : luhnDouble y : doubleEveryOther zs

luhnDouble :: Int -> Int
luhnDouble n = 
  let doubled = n * 2
  in if doubled > 9 then doubled - 9 else doubled
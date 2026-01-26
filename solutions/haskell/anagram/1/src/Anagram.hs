module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter isAnagram xss
  where
    targetLower = map toLower xs
    targetSorted = sort targetLower

    isAnagram :: String -> Bool
    isAnagram candidate = 
      let candidateLower = map toLower candidate
      in
        sort candidateLower == targetSorted
        &&
        candidateLower /= targetLower
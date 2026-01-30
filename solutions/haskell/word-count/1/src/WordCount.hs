module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum)
import Data.List (group, sort)

wordCount :: String -> [(String, Int)]
wordCount xs = 
  let 
    clean ' '  = ' '
    clean '\'' = '\''
    clean c
      | isAlphaNum c = toLower c
      | otherwise    = ' '
    
    tokens = words (map clean xs)
        
    stripQuotes w = reverse . dropWhile (== '\'') . reverse . dropWhile (== '\'') $ w
        
    normalizedTokens = map stripQuotes tokens
    
    finalTokens = filter (not . null) normalizedTokens
  in 
    map (\g -> (head g, length g)) . group . sort $ finalTokens
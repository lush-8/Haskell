module Acronym (abbreviate) where

import Data.Char (isAlpha, isUpper, isLower, toUpper)

abbreviate :: String -> String
abbreviate xs = concatMap extractInitials (words normalized)
  where
    normalized = map (\c -> if c == '-' then ' ' else c) xs

    extractInitials :: String -> String
    extractInitials w = 
      let clean = filter isAlpha w
      in case clean of
        []       -> ""
        (x:rest) -> toUpper x : camelCase x rest

    camelCase :: Char -> String -> String
    camelCase _ [] = []
    camelCase prev (c:next)
      | isUpper c && isLower prev = c : camelCase c next
      | otherwise                 = camelCase c next
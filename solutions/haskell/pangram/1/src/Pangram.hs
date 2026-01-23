module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = all (`elem` lowercased) ['a'..'z']
  where
    lowercased = map toLower text
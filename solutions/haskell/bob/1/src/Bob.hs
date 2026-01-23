module Bob (responseFor) where

import Data.Char (isSpace, isUpper, isAlpha)

responseFor :: String -> String
responseFor xs
  | isSilence                = "Fine. Be that way!"
  | isYelling && isQuestion  = "Calm down, I know what I'm doing!"
  | isYelling                = "Whoa, chill out!"
  | isQuestion               = "Sure."
  | otherwise                = "Whatever."
  where
    trimmed = filter (not . isSpace) xs
    isSilence = null trimmed
    isQuestion = not isSilence && last trimmed == '?'
    letters = filter isAlpha xs
    isYelling = not (null letters) && all isUpper letters
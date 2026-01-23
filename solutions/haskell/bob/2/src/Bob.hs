{-# LANGUAGE OverloadedStrings #-}

module Bob (responseFor) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (isSpace, isAlpha, isUpper)

responseFor :: Text -> Text
responseFor xs
  | isSilence                = "Fine. Be that way!"
  | isYelling && isQuestion  = "Calm down, I know what I'm doing!"
  | isYelling                = "Whoa, chill out!"
  | isQuestion               = "Sure."
  | otherwise                = "Whatever."
  where
    trimmed = T.strip xs
    isSilence = T.null trimmed
    isQuestion = not isSilence && T.last trimmed == '?'
    isYelling = T.any isAlpha xs && T.toUpper xs == xs
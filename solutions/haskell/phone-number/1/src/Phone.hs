module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs = checkNANP cleaned
  where
    digits = filter isDigit xs

    cleaned 
      | length digits == 11 && head digits == '1' = Just (tail digits)
      | length digits == 10 = Just digits
      | otherwise = Nothing

    checkNANP :: Maybe String -> Maybe String
    checkNANP Nothing = Nothing
    checkNANP (Just n)
      | validAreaCode && validExchangeCode = Just n
      | otherwise = Nothing
      where
        validAreaCode = (n !! 0) >= '2' && (n !! 0) <= '9'
        
        validExchangeCode = (n !! 3) >= '2' && (n !! 3) <= '9'
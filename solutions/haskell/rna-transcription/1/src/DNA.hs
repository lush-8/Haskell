module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs = traverse convert xs
  where 
    convert :: Char -> Either Char Char
    convert 'G' = Right 'C'
    convert 'C' = Right 'G'
    convert 'T' = Right 'A'
    convert 'A' = Right 'U'
    convert x = Left x
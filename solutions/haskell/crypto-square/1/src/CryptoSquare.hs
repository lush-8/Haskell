module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

encode :: String -> String
encode xs
  | null normalized = ""
  | otherwise       = unwords transposed
  where
    normalized = filter isAlphaNum (map toLower xs)
    len = length normalized
    c = ceiling (sqrt (fromIntegral len :: Double))
    r = if c * (c - 1) >= len then c - 1 else c
    totalSize = r * c
    padded = normalized ++ replicate (totalSize - len) ' '
    rows = chunksOf c padded
    transposed = transpose rows

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
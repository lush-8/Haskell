module WordSearch (search, CharPos(..), WordPos(..)) where

import Data.List (isPrefixOf)
import Control.Applicative ((<|>))

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

directions :: [(Int, Int)]
directions = [(0,1), (0,-1), (1,0), (-1,0), (1,1), (1,-1), (-1,1), (-1,-1)]

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid words = map (\w -> (w, findWord w)) words
  where
    rows = length grid
    cols = if rows > 0 then length (head grid) else 0

    findWord word = foldl (<|>) Nothing 
      [checkDirection word (CharPos c r) d | r <- [1..rows], c <- [1..cols], d <- directions]

    checkDirection word startPos@(CharPos c r) (dx, dy) =
      let 
        len = length word
        endPos = CharPos (c + (len - 1) * dx) (r + (len - 1) * dy)
        chars = [getCharAt (c + i * dx) (r + i * dy) | i <- [0..len-1]]
      in 
        if all isValid chars && map (\(Just val) -> val) chars == word
        then Just (WordPos startPos endPos)
        else Nothing
        
    getCharAt c r
      | r > 0 && r <= rows && c > 0 && c <= cols = Just ((grid !! (r-1)) !! (c-1))
      | otherwise = Nothing
    
    isValid (Just _) = True
    isValid Nothing  = False
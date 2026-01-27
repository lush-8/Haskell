module Minesweeper (annotate) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate [] = []
annotate board =
  zipWith (zipWith transform) coords board
  where
    height = length board
    width = if null board then 0 else length (head board)

    coords :: [[(Int, Int)]]
    coords = [[(r, c) | c <- [0..width - 1]] | r <- [0..height - 1]]

    transform :: (Int, Int) -> Char -> Char
    transform _ '*' = '*'
    transform (r, c) ' ' =
      case countMines r c of
        0 -> ' '
        n -> intToDigit n
    transform _ x = x 

    countMines :: Int -> Int -> Int
    countMines r c = length (filter isMine neighbours)
      where
        neighbours = [(r + dr, c + dc) | dr <- [-1..1], dc <- [-1..1], (dr, dc) /= (0, 0)]

        isMine (nr, nc) = nr >= 0 && nr < height && nc >= 0 && nc < width && (board !! nr !! nc) == '*'
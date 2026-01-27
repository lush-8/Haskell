module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick [] = []
tick grid = 
  [[nextState r c (cellAt r c) | c <- [0..width - 1]] | r <- [0..height - 1]]
  where
    height = length grid
    width = if null grid then 0 else length (head grid)

    nextState :: Int -> Int -> Int -> Int
    nextState r c currentState
      | currentState == 1 && (neighbours == 2 || neighbours == 3) = 1
      | currentState == 0 && neighbours == 3 = 1
      | otherwise = 0
      where
        neighbours = countLiveNeighbours r c

    countLiveNeighbours :: Int -> Int -> Int
    countLiveNeighbours r c = sum [cellAt(r + dr) (c + dc) | dr <- [-1..1], dc <- [-1..1], (dr, dc) /= (0, 0)]

    cellAt :: Int -> Int -> Int
    cellAt r c
      | r >= 0 && r < height && c >= 0 && c < width = (grid !! r) !! c
      | otherwise = 0
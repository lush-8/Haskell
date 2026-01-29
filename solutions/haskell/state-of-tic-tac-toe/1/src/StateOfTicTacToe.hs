module StateOfTicTacToe (gameState, GameState(..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
  | not (xCount == oCount || xCount == oCount + 1) = Impossible
  | xWon && oWon = Impossible
  | xWon && xCount == oCount = Impossible
  | oWon && xCount > oCount = Impossible
  | xWon = WinX
  | oWon = WinO
  | totalMoves == 9 = Draw
  | otherwise = Ongoing
  where
    flatBoard = concat board
    totalMoves = length (filter (/= ' ') flatBoard)
    xCount = length (filter (== 'X') flatBoard)
    oCount = length (filter (== 'O') flatBoard)

    rows = board
    cols = transpose board
    diags = [[board !! 0 !! 0, board !! 1 !! 1, board !! 2 !! 2], [board !! 0 !! 2, board !! 1 !! 1, board !! 2 !! 0]]
    allLines = rows ++ cols ++ diags
    hasWon player = any (all (== player)) allLines
    xWon = hasWon 'X'
    oWon = hasWon 'O'
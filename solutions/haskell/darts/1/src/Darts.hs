module Darts (score) where

score :: Float -> Float -> Int
score x y
  | distSq <= 1   = 10
  | distSq <= 25  = 5
  | distSq <= 100 = 1
  | otherwise     = 0
  where distSq = x^2 + y^2

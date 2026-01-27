module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | otherwise = Just result
  where
    aliquotSum = sum [x | x <- [1 .. n `div` 2], n `mod` x == 0]

    result 
      | aliquotSum == n = Perfect
      | aliquotSum > n = Abundant
      | aliquotSum < n = Deficient
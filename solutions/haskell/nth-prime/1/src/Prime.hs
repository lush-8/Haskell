module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n <= 0    = Nothing
  | otherwise = Just (primes !! (n - 1))

primes :: [Integer]
primes = 2 : filter isPrime[3, 5..]

isPrime :: Integer -> Bool
isPrime x = all (\p -> x `mod` p /= 0) factors
  where
    factors = takeWhile (\p -> p * p <= x) primes
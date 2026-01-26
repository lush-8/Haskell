module Clock (addDelta, fromHourMin, toString) where

import Text.Printf (printf)

data Clock = Clock Int
  deriving Eq

minutesPerDay :: Int
minutesPerDay = 24 * 60

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock ((hour * 60 + min) `mod` minutesPerDay)

toString :: Clock -> String
toString (Clock totalMinutes) = printf "%02d:%02d" h m
  where
    h = totalMinutes `div` 60
    m = totalMinutes `mod` 60

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock currentMinutes) =
  let delta = hour * 60 + min
  in Clock ((currentMinutes + delta) `mod` minutesPerDay)
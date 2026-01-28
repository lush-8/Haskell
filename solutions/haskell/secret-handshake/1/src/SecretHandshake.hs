module SecretHandshake (handshake) where

import Data.Bits (testBit)

handshake :: Int -> [String]
handshake n = 
  if testBit n 4
    then reverse actions
    else actions
  where
    codes = [(0, "wink"), (1, "double blink"), (2, "close your eyes"), (3, "jump")]

    actions = [action | (i, action) <- codes, testBit n i]
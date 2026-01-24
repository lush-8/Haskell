module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as M

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = fmap count (traverse parse xs)
  where
    parse :: Char -> Either String Nucleotide
    parse 'A' = Right A
    parse 'C' = Right C
    parse 'G' = Right G
    parse 'T' = Right T
    parse _   = Left "Invalid nucleotide in strand"

    count :: [Nucleotide] -> Map Nucleotide Int
    count ns = foldr (\n map -> M.adjust (+1) n map) initialMap ns

    initialMap :: Map Nucleotide Int
    initialMap = M.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

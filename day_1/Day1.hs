module Day1 where
import Data1 (inputIO)

{-============================================================================-}
{-+                                Part One                                  +-}
{-============================================================================-}
deltas :: [Int] -> [Int]
deltas ns = zipWith (-) (tail ns) ns
{-====================================================-}
answer1 :: IO Int
answer1 = inputIO >>= return.length.filter (>0).deltas
{-============================================================================-}
{-+                                Part Two                                  +-}
{-============================================================================-}
deltaSpaces :: Int -> [Int] -> [Int]
deltaSpaces n ks = zipWith (-) (drop n ks) ks
{-====================================================-}
answer2 :: IO Int
answer2 = inputIO >>= return.length.filter (>0).deltaSpaces 3 

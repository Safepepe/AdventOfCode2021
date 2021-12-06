module Day6 where
import Data6 (inputIO)
import Data.List (sort, group)
{-============================================================================-}
{-+                                Part One                                  +-}
{-============================================================================-}
type DaysLeft = Int
type Amount = Int
type Fish = (DaysLeft, Amount)

toTuples :: [DaysLeft] -> [Fish]
toTuples = map (\xs -> (head xs, length xs)).group.sort

accumulate ::  [Fish] -> [Fish]
accumulate fs = let ns = snd.unzip $ filter ((==0).fst) fs in
                (0,sum ns): filter ((/=0).fst) fs

advanceDay :: [Fish] -> [Fish]
advanceDay = accumulate . foldr waitOrReproduce []
 where
   waitOrReproduce :: Fish -> [Fish] -> [Fish]
   waitOrReproduce (day,n) fs
    |day <= 0  = (6,n):(8,n):fs
    |otherwise = (day-1, n):fs

advanceManyDays :: Int -> [Fish] -> [Fish]
advanceManyDays n
  | n > 0     = foldr1 (.)$replicate n advanceDay
  | otherwise = id
{-============================================================================-}
answer1 :: IO Int
answer1 = do
  fs <- inputIO
  let fish0 = toTuples fs
      fish80 = advanceManyDays 80 fish0
      totalFish = sum.snd.unzip$fish80
  return totalFish
{-============================================================================-}
{-+                                Part Two                                  +-}
{-============================================================================-}
answer2 :: IO Int
answer2 = do
  fs <- inputIO
  let fish0 = toTuples fs
      fish256 = advanceManyDays 256 fish0
      totalFish = sum.snd.unzip$fish256
  return totalFish

module Day3 where
import Data3 (inputIO)
import Data.List (sort, group)
{-============================================================================-}
{-+                                Part One                                  +-}
{-============================================================================-}
gammaBase2 :: [[Int]] -> [Int]
gammaBase2 ns = let sumVector = foldr1 (zipWith (+)) ns
               in toDigit.(>(div (length ns) 2))<$>sumVector
  where
    toDigit b = if b then 1 else 0

toBase10 :: [Int] -> Int
toBase10 digitL = sum$zipWith (*) digitL powers
  where
    powers = reverse$(2^)<$>[0..(length digitL -1)]

gammaRate :: [[Int]] -> Int
gammaRate = toBase10.gammaBase2

epsilonRate :: [[Int]] -> Int
epsilonRate = toBase10.map (\x->abs$x-1).gammaBase2
{-====================================================-}
answer1 :: IO Int
answer1 = do ds <- inputIO
             return$ gammaRate ds * epsilonRate ds
{-============================================================================-}
{-+                                Part Two                                  +-}
{-============================================================================-}
modeAt :: Int -> [[Int]] -> Int
modeAt n = head.foldl1 (pickBigger).group.sort.map(!!n)
 where
   pickBigger :: [a] -> [a] -> [a]
   pickBigger zros ones = case compare (length zros) (length ones) of
                           GT -> zros
                           LT -> ones
                           EQ -> ones

oxygenRating :: [[Int]] -> [Int]
oxygenRating xs = head$foldr (filterByMode) xs positions
 where
   positions = let k = length (head xs) -1 in reverse [0..k]
   filterByMode :: Int -> [[Int]] -> [[Int]]
   filterByMode pos rest = let mode = modeAt pos rest
                           in filter ((==mode).(!!pos)) rest

antiModeAt :: Int -> [[Int]] -> Int
antiModeAt n = head.foldl1 (pickSmaller).group.sort.map(!!n)
 where
   pickSmaller :: [a] -> [a] -> [a]
   pickSmaller zros ones = case compare (length zros) (length ones) of
                            GT -> ones
                            LT -> zros
                            EQ -> zros

cO2Rating :: [[Int]] -> [Int]
cO2Rating xs = head$foldr (filterByAntiMode) xs positions
 where
   positions = let k = length (head xs) -1 in reverse [0..k]
   filterByAntiMode :: Int -> [[Int]] -> [[Int]]
   filterByAntiMode pos rest = let antiMode = antiModeAt pos rest
                               in filter ((==antiMode).(!!pos)) rest
{-====================================================-}
answer2 :: IO Int
answer2 = do ds <- inputIO
             let cO2 = toBase10$cO2Rating ds
                 o2  = toBase10$oxygenRating ds
             return$ o2 * cO2

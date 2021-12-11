module Day9 where
import Data9 (inputIO)
import Data.List (group, transpose,unfoldr, sort)
{-============================================================================-}
{-                             Part One                                       -}
{-============================================================================-}
type Counter = Int
type Position = (Int,Int)

deltas :: [Int] -> [Int]
deltas []  = []
deltas [x] = []
deltas xs  = zipWith (-) (tail xs) xs

minima :: [Int] -> [Bool]
minima xs = (==2)<$>fPrimePrime
 where
   fPrime = map signum.deltas$(10:xs)++[10]
   fPrimePrime = deltas fPrime

minBoolMap :: [[Int]] -> [[Bool]]
minBoolMap xs = zipWith (zipWith (&&)) minRows minColumns
  where
    minRows = minima<$>xs
    minColumns = transpose$minima<$>(transpose xs)

lowPoints :: [[Int]] -> [Int]
lowPoints xs = map snd.filter (fst).concat$zipWith (zip) (minBoolMap xs) xs
{-============================================================================-}
answer1 :: IO Int
answer1 = do nsMap <- inputIO
             return.sum$(+1)<$>(lowPoints nsMap)
{-============================================================================-}
{-                             Part Two                                       -}
{-============================================================================-}
adjecentFlow :: [[Int]] -> [Position] -> Maybe ([Position],[Position])
adjecentFlow nsMap [] = Nothing
adjecentFlow nsMap ps = Just (nextPos, nextPos)
 where
   nextPos = concat.map (biggerAdjecents nsMap)$ps

biggerAdjecents :: [[Int]] -> Position -> [Position]
biggerAdjecents nMap (x,y) = filter (`biggerThan`(x,y)) adjecents
  where
    adjecents = filter inBounds [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    inBounds :: Position -> Bool
    inBounds (x0,y0) = x0>=0 && x0<(length.head$nMap) && y0>=0 && y0<length nMap
    biggerThan :: Position -> Position -> Bool
    biggerThan (x1,y1) (x0,y0) = if ((nMap!!y1)!!x1) == 9 then
                                   False
                                 else
                                   ((nMap!!y1)!!x1) > ((nMap!!y0)!!x0)

basin :: [[Int]] -> Position -> [Position]
basin nsMap (x,y) = foldr avoidDuplicates [(x,y)].concat$ unfoldr (adjecentFlow nsMap) [(x,y)]
  where
    avoidDuplicates x xs = if x`elem`xs then xs else x:xs

lowPositions :: [[Int]] -> [Position] --Broken
lowPositions nsMap =  map snd.filter fst.concat$zipWith (zip) (minBoolMap nsMap) coords
  where
    coords = zipWith zip (replicate (ny+1) [0..nx]) (transpose$replicate (nx+1) [0..ny])
    nx = (length.head$nsMap) -1
    ny = (length$nsMap) -1

example :: [[Int]]
example =[[2,1,9,9,9,4,3,2,1,0],[3,9,8,7,8,9,4,9,2,1],[9,8,5,6,7,8,9,8,9,2],[8,7,6,7,8,9,6,7,8,9],[9,8,9,9,9,6,5,6,7,8]]
{-============================================================================-}
answer2 :: IO Int
answer2 = do nsMap <- inputIO
             let lowPos = lowPositions nsMap
                 basins = (basin nsMap)<$>lowPos
                 basinSizes = reverse.sort$length<$>basins
                 threeLargestBasinSizes = take 3 basinSizes
             return.foldr1 (*)$threeLargestBasinSizes

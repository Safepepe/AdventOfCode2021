module Day5 where
import Data5 (inputIO, inputTest)
import GHC.Exts (sortWith)
import Data.List (groupBy, group)
{-============================================================================-}
{-+                                Part One                                  +-}
{-============================================================================-}
type Position = (Int,Int)
type Line = (Position,Position)

verticalOrHorizontal :: [Line] -> [Line]
verticalOrHorizontal = filter (\x -> isHorizontal x || isVertical x)
 where
   isVertical ((x1,y1),(x2,y2)) = x1 == x2
   isHorizontal ((x1,y1),(x2,y2)) = y1 == y2

expandLine :: Line -> [Position]
expandLine ((x1,y1),(x2,y2))
   | x1 < x2 && y1 < y2   = (x1,y1): expandLine ((x1+1,y1+1),(x2,y2))
   | x1 < x2 && y1 > y2   = (x1,y1): expandLine ((x1+1,y1-1),(x2,y2))
   | x1 > x2 && y1 < y2   = (x1,y1): expandLine ((x1-1,y1+1),(x2,y2))
   | x1 > x2 && y1 > y2   = (x1,y1): expandLine ((x1-1,y1-1),(x2,y2))
   | y1 < y2 {-x1 == x2-} = (x1,y1): expandLine ((x1  ,y1+1),(x2,y2))
   | y1 > y2 {-x1 == x2-} = (x1,y1): expandLine ((x1  ,y1-1),(x2,y2))
   | x1 < x2 {-y1 == y2-} = (x1,y1): expandLine ((x1+1,y1  ),(x2,y2))
   | x1 > x2 {-y1 == y2-} = (x1,y1): expandLine ((x1-1,y1  ),(x2,y2))
   | x1 == x2 && y1 == y2 = [(x1,y1)]

markedLines :: [Line] -> [Position]
markedLines = concatMap expandLine

markPositions :: [Position] -> [Int]
markPositions = map length. groupPoints
  where
    groupPoints = concatMap (group.sortWith fst).groupBy sameY.sortWith snd
    sameY :: Position -> Position -> Bool
    sameY (x1,y1) (x2,y2) = y1==y2
{-============================================================================-}
answer1 :: IO Int
answer1 = do lns <- inputIO
             let horiVertiLines = markedLines.verticalOrHorizontal$ lns
                 points = markPositions horiVertiLines
                 twoPoints = filter (>1) points
             return$ length twoPoints
{-============================================================================-}
{-+                                Part Two                                  +-}
{-============================================================================-}

{-============================================================================-}
answer2 :: IO Int
answer2 = do lns <- inputIO
             let allLines = markedLines lns
                 points = markPositions allLines
                 twoPoints = filter ((>1)) points
             return$ length twoPoints

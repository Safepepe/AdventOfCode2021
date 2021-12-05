module Day5 where
import Data5 (inputIO)
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

expandHoriVerti :: Line -> [Position]
expandHoriVerti ((x1,y1),(x2,y2))
   | x1 < x2              = (x1,y1): expandHoriVerti ((x1+1,y1),(x2,y2))
   | x1 > x2              = (x1,y1): expandHoriVerti ((x1-1,y1),(x2,y2))
   | y1 < y2              = (x1,y1): expandHoriVerti ((x1,y1+1),(x2,y2))
   | y1 > y2              = (x1,y1): expandHoriVerti ((x1,y1-1),(x2,y2))
   | x1 == x2 && y1 == y2 = [(x1,y1)]

markedHorVerLines :: [Line] -> [Position]
markedHorVerLines = concatMap expandHoriVerti .verticalOrHorizontal

markPositions :: [Position] -> [(Position,Int)]
markPositions = map toTuple. groupPointsTogether
  where
    groupPointsTogether = concat.map group.map (sortWith fst).groupBy sameYCoord.sortWith snd
    toTuple :: [a] -> (a,Int)
    toTuple xs = (head xs , length xs)
    sameYCoord :: Position -> Position -> Bool
    sameYCoord (x1,y1) (x2,y2) = y1==y2
{-============================================================================-}
answer1 :: IO Int
answer1 = do lns <- inputIO
             let horiVertiLines = markedHorVerLines lns
                 points = markPositions horiVertiLines
                 twoPoints = filter ((>1).snd) points
             return$ length twoPoints
{-============================================================================-}
{-+                                Part Two                                  +-}
{-============================================================================-}
expandLine :: Line -> [Position]
expandLine ((x1,y1),(x2,y2))
   | x1 < x2 && y1 < y2   = (x1,y1): expandLine ((x1+1,y1+1),(x2,y2))
   | x1 < x2 && y1 > y2   = (x1,y1): expandLine ((x1+1,y1-1),(x2,y2))
   | x1 > x2 && y1 < y2   = (x1,y1): expandLine ((x1-1,y1+1),(x2,y2))
   | x1 > x2 && y1 > y2   = (x1,y1): expandLine ((x1-1,y1-1),(x2,y2))
   | y1 < y2 {-x1 == x2-} = (x1,y1): expandLine ((x1  ,y1+1),(x2,y2))
   | y1 > y2 {-x1 == x2-} = (x1,y1): expandLine ((x1  ,y1-1),(x2,y2))
   | x1 < x2 {-y1 == y2-} = (x1,y1): expandLine ((x1+1,y1  ),(x2,y2))
   | x1 > x2 {-y1 == y2-} = (x1,y1): expandLine ((x1-1,y1-1),(x2,y2))
   | x1 == x2 && y1 == y2 = [(x1,y1)]

markedLines :: [Line] -> [Position]
markedLines = concatMap expandLine

{-============================================================================-}
answer2 :: IO Int
answer2 = do lns <- inputIO
             let allLines = markedLines lns
                 points = markPositions allLines
                 twoPoints = filter ((>1).snd) points
             return$ length twoPoints

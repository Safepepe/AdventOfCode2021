module Day10 where
import Data10 (inputIO)
import Data.List (sort)
{-============================================================================-}
{-                             Part One                                       -}
{-============================================================================-}
type Line = String

close :: Char -> Char
close '(' = ')'
close '{' = '}'
close '[' = ']'
close '<' = '>'

isOpen :: Char -> Bool
isOpen '(' = True
isOpen '{' = True
isOpen '[' = True
isOpen '<' = True
isOpen  _  = False

valueOfClosed :: Char -> Int
valueOfClosed ')' = 3
valueOfClosed ']' = 57
valueOfClosed '}' = 1197
valueOfClosed '>' = 25137

isCorrupted :: Line -> (Bool,[Char])
isCorrupted = foldl notInQueue (False,[])
  where
    notInQueue :: (Bool,[Char]) -> Char -> (Bool,[Char])
    notInQueue (corrupt, queue) ch
      |corrupt          = (corrupt,queue)
      |isOpen ch        = (corrupt, close ch : queue)
      |null queue       = (True, [ch])
      |ch == head queue = (corrupt, tail queue)
      |otherwise        = (True, [ch])
{-============================================================================-}
answer1 :: IO Int
answer1 = do lns <- inputIO
             let corruptedChars = map (head.snd).filter fst$isCorrupted<$>lns
                 score = sum$valueOfClosed<$>corruptedChars
             return score
{-============================================================================-}
{-                             Part Two                                       -}
{-============================================================================-}
type Score = Int

valueOfQueue :: String -> Score
valueOfQueue = foldl iterateScore 0
  where
    iterateScore :: Score -> Char -> Score
    iterateScore scr ch = 5*scr + valueOf ch
    valueOf :: Char -> Int
    valueOf ')' = 1
    valueOf ']' = 2
    valueOf '}' = 3
    valueOf '>' = 4
{-============================================================================-}
answer2 :: IO Int
answer2 = do lns <- inputIO
             let lineClosers = map snd.filter (not.fst)$isCorrupted<$>lns
                 score = (!!(length lineClosers `div`2)).sort.map valueOfQueue$lineClosers
             return score

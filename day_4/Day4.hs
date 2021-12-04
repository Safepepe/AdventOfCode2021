module Day4 where
import Data4 (inputIO)

{-============================================================================-}
{-+                                Part One                                  +-}
{-============================================================================-}
type Table = [[Int]]
type BingoTable = [[(Int,Bool)]]

toBingoTable :: Table -> BingoTable
toBingoTable = map.map$ (\x -> (x,False))

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose (x:xs) = foldr (zipWith (:)) (replicate (length x) []) (x:xs)

markNumber :: [BingoTable] -> Int -> [BingoTable]
markNumber btbs n = map (matchTable n) btbs
 where
   matchTable ::  Int -> BingoTable -> BingoTable
   matchTable q bt = (map$matchOne q)<$>bt
   matchOne :: Int -> (Int,Bool) -> (Int,Bool)
   matchOne q (x,b) = if x==q then (x,True) else (x,b)

bingo :: BingoTable -> Bool
bingo bt = any isFilled rowsNcolumns
 where
   rowsNcolumns = bt ++ transpose bt
   isFilled :: [(Int,Bool)] -> Bool
   isFilled = all snd

foldWhile :: (b -> Bool) -> (b -> a -> b) -> b -> [a] -> b
foldWhile cond f x []     = x
foldWhile cond f x (y:ys) = if cond x then
                              foldWhile cond f (f x y) ys
                            else
                              x
{-============================================================================-}
answer1 :: IO Int
answer1 = do
  (ns, tbs) <- inputIO
  let bingTbs = toBingoTable<$>tbs
      (tbs2,lastN) = foldWhile thereIsnoWinner mark (bingTbs,undefined) ns
      mark :: ([BingoTable],Int) -> Int -> ([BingoTable],Int)
      mark = \(xs,_) q -> (markNumber xs q , q)
      thereIsnoWinner :: ([BingoTable],Int) -> Bool
      thereIsnoWinner = (all$not.bingo).fst
      winTable = head.filter bingo$ tbs2
      unmarkedSum = sum.map fst.filter (not.snd).concat$ winTable
   return$lastN * unmarkedSum
{-============================================================================-}
{-+                                Part Two                                  +-}
{-============================================================================-}

{-============================================================================-}
answer2::  IO Int
answer2 = do (ns, tbs) <- inputIO
             let bingTbs = toBingoTable<$>tbs
                 markNFilter :: [BingoTable] -> Int -> [BingoTable]
                 markNClean = \xs q -> filter (not.bingo) (markNumber xs q)
                 resetBools :: BingoTable -> BingoTable
                 resetBools = map$ map (\(x,b)-> (x,False))
                 lastTable = resetBools.head$foldWhile ((>1).length) markNClean binTbs ns
                 mark = \(xs,_) q  -> (markNumber xs q , q)
                 (tbs2,lastN) = foldWhile ((all$not.bingo).fst) (mark) ([lastTable],undefined) ns
                 lastWin = head$tbs2
                 unmarkedSum = sum.map fst.filter (not.snd).concat$ lastWin
             return$lastN * unmarkedSum

module Day13 where
import Data13 (inputIO)
import GHC.Exts (sortWith)
import Data.List (groupBy, transpose)
{-============================================================================-}
{-                             Part One                                       -}
{-============================================================================-}
type Point = (Int,Int)
type Fold = (Char, Int)
type Origami = [Point]

symmetry ::  Fold -> Point -> Point
symmetry (xOrY, n) (x,y)
  |xOrY == 'y' && y <= n = (x,y)
  |xOrY == 'y' && y > n = (x, 2*n - y)
  |xOrY == 'x' && x <= n = (x,y)
  |xOrY == 'x' && x > n = (2*n -x , y)

getVisible :: [Point] -> [Point]
getVisible = foldr (\x y -> if x`elem`y then y else x:y) []

applyFold :: Fold -> Origami -> Origami
applyFold fld = getVisible.map (symmetry fld)
{-============================================================================-}
answer1 :: IO Int
answer1 = do (pts, flds) <- inputIO
             let fstFold = head flds
                 ptsAfterFold = applyFold fstFold pts
             return$length ptsAfterFold

{-============================================================================-}
{-                             Part Two                                       -}
{-============================================================================-}

showOrigami :: Origami -> String
showOrigami pts = unlines.map (map fillIn)$emptyOrigami
  where
    xmin = foldr min 0.map fst$pts
    xmax = foldr max 0.map fst$pts
    ymin = foldr min 0.map snd$pts
    ymax = foldr max 0.map snd$pts
    plotablePoints :: Origami
    plotablePoints = map (\(x,y) -> (x+xmin, y+ymin)) pts
    emptyOrigami :: [[Point]]
    emptyOrigami = zipWith zip (replicate (ymax+1) [0..xmax]) (transpose$replicate (xmax+1) [0..ymax])
    fillIn :: Point -> Char
    fillIn p = if p`elem`plotablePoints then '#' else '.'


{-============================================================================-}
answer2 :: IO ()
answer2 = do (pts, flds) <- inputIO
             let ptsAfterFold = foldr1 (.) (reverse$applyFold<$>flds)$pts
             putStr$showOrigami$ptsAfterFold

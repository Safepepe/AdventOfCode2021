module Day15 where
import Data15 (inputIO)
import Control.Monad.Reader
import qualified Data.HashMap.Strict as M
import Data.List (sortBy,transpose)
{-============================================================================-}
{-                             Part One                                       -}
{-============================================================================-}
type Risk = Int
type RiskMap = M.HashMap Position Risk
type Cave = Reader RiskMap
type Position = (Int,Int)
type Distance = Int
type Node = (Position, Maybe (Distance, Position))
type VisitedNodes = M.HashMap Position (Maybe (Distance, Position))
type PriorityQueue = [Node]

risk :: Position -> Cave (Maybe Risk)
risk (x,y) = ask >>= return. M.lookup (x,y)

relax :: Node -> Node -> Cave (Maybe Node)
relax (p0, Just (d0,_)) (p1, mInfo) =
  do maybeRsk <- risk p1
     case (maybeRsk, mInfo) of
       (Nothing, _ )           -> return $ Nothing
       (Just r , Nothing)      -> return $ Just (p1, Just (d0+r,p0))
       (Just r , Just(d1,pr1)) -> if d1 < d0 + r then
                                    return $ Just (p1, Just (d1, pr1))
                                  else
                                    return $ Just (p1, Just (d0+r, p0))

dijkstra :: VisitedNodes -> PriorityQueue -> Cave VisitedNodes
dijkstra vs [] = return vs
dijkstra vs (u:rest) = cleanQueue >>= dijkstra (M.insert (x0,y0) m vs)
  where
    ((x0,y0), m) = u
    neighbors = [((x0+1,y0),Nothing),((x0-1,y0),Nothing),((x0,y0+1),Nothing),((x0,y0-1),Nothing)]
    dirtyQueue :: [Node]
    dirtyQueue = foldr addTo rest.filter (not.(`M.member`vs).fst)$neighbors
    addTo :: Node -> PriorityQueue -> PriorityQueue
    addTo nd ls = if fst nd `elem` (fst<$>ls) then ls else nd:ls
    cleanQueue :: Cave [Node]
    cleanQueue = (sortBy compareFunc.unJust.sequence.filter (/=Nothing))<$>(mapM (relax u) dirtyQueue)
    unJust :: Maybe a -> a
    unJust (Just x) = x
    unJust Nothing  = error "applied unJust to Nothing"
    compareFunc :: Node -> Node -> Ordering
    compareFunc (p0,_ ) (p1,Nothing) = LT
    compareFunc (p0,Nothing) (p1,_) = GT
    compareFunc (p0,Just (d0,pr0)) (p1,Just (d1,pr1)) = compare d0 d1

initQueue :: PriorityQueue
initQueue = [((0,0), Just (0, (0,0)))]

initKeyValueList :: [[Risk]] -> [(Position,Risk)]
initKeyValueList riskMtx = concat $ zipWith zip keyMtx riskMtx
  where
    keyMtx = zipWith zip xmtx ymtx
    xmtx = replicate ny [0..(nx-1)]
    ymtx = transpose $ replicate nx [0..(ny-1)]
    nx = length.head$riskMtx
    ny = length riskMtx
{-============================================================================-}
answer1 :: IO (Maybe Distance)
answer1 = do crudeRiskMap <- inputIO
             let riskMap = initKeyValueList crudeRiskMap
                 dijkstraMap = runReader (dijkstra M.empty initQueue) $ M.fromList riskMap
                 nx = length crudeRiskMap
                 ny = length.head$crudeRiskMap
                 endDistance = fmap fst.join $ M.lookup (nx-1,ny-1) dijkstraMap
             return$ endDistance
{-============================================================================-}
{-                             Part Two                                       -}
{-============================================================================-}
increaseRisk :: [[Risk]] -> [[Risk]]
increaseRisk = map $ map (\n -> if n < 9 then n+1 else 1)

expandTileRight :: Int -> [[Risk]] -> [[Risk]]
expandTileRight n
  | n <= 0    = const []
  | otherwise = foldl1 (\x y-> zipWith (++) x y) . take n . iterate increaseRisk

expandTileDown :: Int -> [[Risk]] -> [[Risk]]
expandTileDown n
  | n <= 0    = const []
  | otherwise = foldr1 (++) . take n . iterate increaseRisk

expand :: Int -> [[Risk]] -> [[Risk]]
expand n = expandTileDown n . expandTileRight n
{-============================================================================-}
answer2 :: IO (Maybe Distance)
answer2 = do crudeRiskMap <- inputIO
             let riskMap = initKeyValueList$ expand 5 crudeRiskMap
                 dijkstraMap = runReader (dijkstra M.empty initQueue) $ M.fromList riskMap
                 nx = 5 * (length crudeRiskMap)
                 ny = 5 * (length.head$crudeRiskMap)
                 endDistance = fmap fst.join $ M.lookup (nx-1,ny-1) dijkstraMap
             return$ endDistance

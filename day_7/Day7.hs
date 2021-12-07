module Day7 where
import Data7 (input)
import GHC.Exts(sortWith)
{-============================================================================-}
{-                             Part One                                       -}
{-============================================================================-}
type Position = Int
type Cost = Int
type Crabs = [Position] --a Crab is a position in the list.

costFunc :: Position -> Cost-- it's a convex function ;D
costFunc p = sum.map distanceToP$ input
 where
   distanceToP x = abs$ p-x

bestPositionWith :: (Position -> Cost) -> Position
bestPositionWith f = fst.head$ sortWith snd costs
  where
    pmin = foldr1 min input
    pmax = foldr1 max input
    ps = [pmin .. pmax]
    costs = zip ps $ f <$> ps
{-============================================================================-}
answer1 :: Int
answer1 = costFunc bestPos
 where
   bestPos = bestPositionWith costFunc
{-============================================================================-}
{-                             Part One                                       -}
{-============================================================================-}
costFunc2 :: Position -> Cost-- it's a convex function ;D
costFunc2 p = sum.map distanceToP$ input
 where
   distanceToP x = abs$(p-x)*(abs(p-x)+1)`div`2
{-============================================================================-}
answer2 :: Int
answer2 = costFunc2 bestPos
 where
   bestPos = bestPositionWith costFunc2

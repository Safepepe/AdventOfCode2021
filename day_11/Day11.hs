module Day11 where
import Data11 (input)
import Data.List (transpose, unfoldr)
import Control.Monad.State.Lazy

{-============================================================================-}
{-                             Part One                                       -}
{-============================================================================-}
type Cave = [[Int]]
type Octopi = State Cave
type Position = (Int,Int)
type NumberOfFlashes = Int

inBounds :: Position -> Octopi Bool
inBounds (x0,y0) = do
                   cave <- get
                   return $ x0>=0 && y0>=0 && x0<(length.head$cave) && y0<length cave

addOneAt :: Position -> Octopi () --unsafe.
addOneAt (x,y) = do
                 cv <- get
                 put $ take y cv ++ (zipWith (+) (cv!!y) (replicate x 0 ++ 1:repeat 0)) : drop (y+1) cv

resetEnergyAt :: Position -> Octopi () --unsafe
resetEnergyAt (x,y) = do
                      cv <- get
                      put $ take y cv ++ (zipWith (*) (cv!!y) (replicate x 1 ++ 0:repeat 1)) : drop (y+1) cv

adjecentsTo :: Position -> Octopi [Position]
adjecentsTo (x,y) = filterM inBounds [(x+1,y),(x-1,y),(x,y+1),(x,y-1),(x+1,y+1),(x-1,y-1),(x-1,y+1),(x+1,y-1)]

addOneAround :: Position -> Octopi ()
addOneAround xy = (adjecentsTo xy) >>= (mapM_ addOneAt)

cavePositions :: (Int -> Bool) -> Octopi [Position]
cavePositions condtn = do
                       cave <- get
                       let boolMap  = map (condtn<$>) cave
                           nx       = (length.head$cave) -1
                           ny       = (length$cave) -1
                           coordMap = zipWith zip (replicate (ny+1) [0..nx]) (transpose$replicate (nx+1) [0..ny])
                       return$map snd.filter fst.concat$zipWith zip boolMap coordMap

flashersNotIn :: [Position] -> Octopi [Position]
flashersNotIn ps = (foldr (avoid ps) []) <$> cavePositions (>9)
  where
    avoid :: [Position] -> Position -> [Position] -> [Position]
    avoid qs xy newPs = if xy`elem`qs then newPs else xy:newPs

flashCascade :: [Position] -> Octopi [Position]
flashCascade pastFlashers = do
                   newFlashers <- flashersNotIn pastFlashers
                   if null newFlashers then
                      return pastFlashers
                   else
                      mapM_ addOneAround newFlashers >> flashCascade (newFlashers ++ pastFlashers)

step :: Octopi NumberOfFlashes
step = do addOneToAll
          flashers <- flashCascade []
          (cavePositions (>9) >>= mapM_ resetEnergyAt)
          return.length$flashers
  where
    addOneToAll = get >>= put.map ((+1)<$>)
{-============================================================================-}
answer1 :: Int
answer1 =  evalState (fmap sum.sequence$replicate 100 step) input
{-============================================================================-}
{-                             Part Two                                       -}
{-============================================================================-}
allAreZero :: Octopi Bool
allAreZero = do ps <- cavePositions (==0)
                cv <- get
                return$ length ps == (length.concat$cv)

synchronize :: Int -> Octopi Int
synchronize n =  do
                 step
                 synced <- allAreZero
                 if synced then
                   return$n+1
                 else
                   synchronize$n+1
{-============================================================================-}
answer2 :: Int
answer2 = evalState (synchronize 0) input

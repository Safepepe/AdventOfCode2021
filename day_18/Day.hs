module Day where
import Data (inputIO, SnailNumber(Leaf, Node), Choice(L,R), testParse)
import Control.Applicative
{-============================================================================-}
{-                             Part One                                       -}
{-============================================================================-}
type Position = [Choice] -- is finite
type Neighbor = [Choice] -- is infinite
type LeftValue = Int
type RightValue = Int


joinNumbers :: SnailNumber -> SnailNumber -> SnailNumber
joinNumbers s1 s2 = Node [] s1 s2

relabel :: SnailNumber -> SnailNumber
relabel s1 = relabelFrom [] s1
  where
    relabelFrom :: Position -> SnailNumber -> SnailNumber
    relabelFrom currPos (Leaf _ n)   = Leaf currPos n
    relabelFrom currPos (Node _ l r) = let lPos = currPos ++ [L]
                                           rPos = currPos ++ [R]
                                       in Node currPos (relabelFrom lPos l) (relabelFrom rPos r)

fuse :: SnailNumber -> SnailNumber -> SnailNumber
fuse s1 s2 = relabel $ joinNumbers s1 s2

explodingNode :: SnailNumber ->  Maybe (Position, LeftValue, RightValue)
explodingNode (Node pos (Leaf _ n1) (Leaf _ n2)) = if length pos >= 4 then Just (pos, n1, n2) else Nothing
explodingNode (Node pos s1 s2) = explodingNode s1 <|> explodingNode s2
explodingNode _  = Nothing

rightNeighborOf :: Position -> Maybe Neighbor
rightNeighborOf p = if null.filter(==L)$p then Nothing else Just neighbor
  where
    neighbor = (reverse.rightOf.reverse$p) ++ repeat L
    rightOf :: Position -> Neighbor
    rightOf (L:rest) = R:rest
    rightOf (R:rest) = L:(rightOf rest)
    rightOf []       = [] --This shouldn't happen

leftNeighborOf :: Position -> Maybe Neighbor
leftNeighborOf p = if null.filter(==R)$p then Nothing else Just neighbor
  where
    neighbor = (reverse.leftOf.reverse$p) ++ repeat R
    leftOf :: Position -> Neighbor
    leftOf (R:rest) = L:rest
    leftOf (L:rest) = R:(leftOf rest)
    leftOf []       = [] --This shouldn't happen

modifyNeighbor :: Maybe Neighbor -> (SnailNumber -> SnailNumber) -> SnailNumber -> SnailNumber
modifyNeighbor Nothing _ s1   = s1
modifyNeighbor _ f (Leaf p n) = f (Leaf p n)
modifyNeighbor (Just (p:ps)) f (Node c l r)
  | p == L = Node c (modifyNeighbor (Just ps) f l) r
  | p == R = Node c l (modifyNeighbor (Just ps) f r)
modifyNeighbor (Just []) f s = error "Some neighbor value is not infinite!" --f s -- This should never happen

modifyNode :: Position -> (SnailNumber -> SnailNumber) -> SnailNumber -> SnailNumber
modifyNode (p:ps) f (Node c l r)
                  | p == L  = Node c (modifyNode ps f l) r
                  | p == R  = Node c l (modifyNode ps f r)
modifyNode [] f s           = f s -- This should be the only one that happens
modifyNode _  f (Leaf p n)  = error "modifyNode is failing" -- f (Leaf p n) -- This should never happen

replaceByZero :: SnailNumber -> SnailNumber
replaceByZero (Leaf p n)   = Leaf p 0
replaceByZero (Node p l r) = Leaf p 0

addNum :: Int -> SnailNumber -> SnailNumber
addNum n (Node p l r) = error "adding number to a pair"
addNum n (Leaf p n1)  = Leaf p (n1 + n)

explode :: SnailNumber -> SnailNumber
explode s = case explodingNode s of
              Nothing              -> s
              Just (p, lval, rval) -> let lNeighbor  = leftNeighborOf$p ++ [L]
                                          rNeighbor  = rightNeighborOf$p ++ [R]
                                          addToRight = modifyNeighbor rNeighbor (addNum rval)
                                          addToLeft  = modifyNeighbor lNeighbor (addNum lval)
                                          removeNode = modifyNode p replaceByZero
                                      in  removeNode.addToRight.addToLeft$s

splittingNode :: SnailNumber ->  Maybe Position
splittingNode (Node pos s1 s2) = splittingNode s1 <|> splittingNode s2
splittingNode (Leaf p n) = if n > 9 then Just p else Nothing -- only values can split


split :: SnailNumber -> SnailNumber
split s = case splittingNode s of
            Nothing -> s
            Just p  -> modifyNode p splitNode s
  where
    splitNode :: SnailNumber -> SnailNumber
    splitNode (Node p r l) = error "applying splitNode to something other than a value!"
    splitNode (Leaf p n)
       | even n     = Node p (Leaf (p ++ [L]) (n`div`2)) (Leaf (p ++ [R]) (n`div`2))
       | otherwise  = Node p (Leaf (p ++ [L]) (n`div`2)) (Leaf (p ++ [R]) ((n+1)`div`2))

simplify :: SnailNumber -> SnailNumber
simplify s = case (explodingNode s, splittingNode s) of
               (Nothing, Nothing) -> s
               (Nothing, _      ) -> simplify $ split s
               ( _     , _      ) -> simplify $ explode s

addSnails :: SnailNumber -> SnailNumber -> SnailNumber
addSnails s1 s2 = simplify $ fuse s1 s2

magnitude :: SnailNumber -> Int
magnitude (Leaf _ n) = n
magnitude (Node _ l r) = 3 * (magnitude l) + 2 * (magnitude r)

tests :: [SnailNumber]
tests = testParse <$> ["[1,1]","[2,2]","[3,3]","[4,4]"]

{-============================================================================-}
answer1 :: IO Int
answer1 = do snls <- inputIO
             return $ magnitude $ foldl1 addSnails snls
{-============================================================================-}
{-                             Part Two                                       -}
{-============================================================================-}
pairMagnitude :: (SnailNumber, SnailNumber) -> Int
pairMagnitude (s1,s2) = magnitude $ addSnails s1 s2

pairUpList :: Eq a => [a] -> [(a,a)]
pairUpList xs = [(x,y)| x<-xs, y <-xs, not$x==y]
{-============================================================================-}
answer2 :: IO Int
answer2 = do snls <- inputIO
             let pairs = pairUpList snls
                 mags = pairMagnitude <$> pairs
             return $ foldr1 max mags

module Day12 where
import Data12 (input)
import Data.Char (isLower, isUpper)
import Data.List (intersperse)
import Control.Monad.Reader
import Control.Monad.State.Lazy
{-============================================================================-}
{-                             Part One                                       -}
{-============================================================================-}
type Cave = String
type Connection = (Cave,Cave)
type Path = [Cave]
data PathsTree =  Node Path [PathsTree]
   deriving(Eq)
type Submarine = Reader [Connection]

showPaths :: PathsTree -> [Path]
showPaths (Node pth []) = [pth]
showPaths (Node pth trs) = concatMap showPaths trs

instance Show PathsTree where
  show = unlines.map (concat.reverse.intersperse " -> ").showPaths

start :: PathsTree
start = Node ["start"] []

isLowerCase :: Cave -> Bool
isLowerCase = or.map isLower

isUpperCase :: Cave -> Bool
isUpperCase = or.map isUpper


validNext :: Path  -> Cave -> Bool
validNext pth newCave
  | isLowerCase newCave && newCave`elem`pth = False
  | otherwise                               = True

cavesConnectedTo :: Cave -> [Connection] -> [Cave]
cavesConnectedTo cave = map snd . filter ((==cave).fst)

advance :: PathsTree -> Submarine PathsTree
advance (Node pth trs)
  |head pth == "end" = return $ Node pth trs
  |not.null$trs      = fmap (Node pth) $ mapM advance trs
  |otherwise         = do
                       adjecentCaves <- mapReader (cavesConnectedTo$head pth) ask
                       let validNexts = filter (pth`validNext`) adjecentCaves
                           newpths = (:pth)<$>validNexts
                           newtrs = zipWith ($) (Node<$>newpths) $ repeat []
                       return $ Node pth newtrs

allPaths :: PathsTree -> Submarine PathsTree
allPaths (Node pth trs)
  |not.null$trs = fmap (Node pth) $ mapM allPaths trs
  |otherwise    = do newTree <- advance (Node pth trs)
                     if newTree == (Node pth trs) then
                       return (Node pth trs)
                     else
                       allPaths newTree

{-============================= Testing ======================================-}
example :: [Connection]
example = example0 ++ map (\(x,y) -> (y,x)) example0
  where
    example0 =[("start","A"),("start","b"),("A","c"),("A","b"),("b","d"),("A","end"),("b","end")]

test :: Submarine a -> a
test sub = runReader sub example
{-============================================================================-}
answer1 :: Int
answer1 = length . filter ((=="end").head) . showPaths $ runReader (allPaths start) input
{-============================================================================-}
{-                             Part Two                                       -}
{-============================================================================-}
validPath :: Path -> Bool
validPath xs = length lowers - (length$lowerNoCopies) < 2
  where
    lowers = filter (isLowerCase) xs
    lowerNoCopies = foldr (\x y -> if x`elem`y then y else x:y) [] lowers

validNext2 :: Path  -> Cave -> Bool
validNext2 pth newCave
  |isUpperCase newCave     = True
  |newCave == "start"      = False
  |validPath (newCave:pth) = True
  |otherwise               = False

advance2 :: PathsTree -> Submarine PathsTree
advance2 (Node pth trs)
  | head pth == "end" = return $ Node pth trs
  | not.null$trs      = fmap (Node pth) $ mapM advance2 trs
  | otherwise         = do
                        adjecentCaves <- mapReader (cavesConnectedTo$head pth) ask
                        let validNexts = filter (validNext2 pth) adjecentCaves
                            newpths = (:pth)<$>validNexts
                            newtrs = zipWith ($) (Node<$>newpths) $ repeat []
                        return $ Node pth newtrs

allPaths2 :: PathsTree -> Submarine PathsTree
allPaths2 (Node pth trs)
  |not.null$trs = fmap (Node pth) $ mapM allPaths2 trs
  |otherwise    = do newTree <- advance2 (Node pth trs)
                     if newTree == (Node pth trs) then
                       return (Node pth trs)
                     else
                       allPaths2 newTree
{-============================================================================-}
answer2 :: Int
answer2 = length . filter ((=="end").head) . showPaths $ runReader (allPaths2 start) input

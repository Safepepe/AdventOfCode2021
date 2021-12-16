module Day14 where
import Data14 (inputIO)
import Control.Monad.Reader
import Data.List (sort,group)
import GHC.Exts (sortWith)
{-============================================================================-}
{-                             Part One                                       -}
{-                             Part Two                                       -}
{-============================================================================-}
type Polymer = String
type Result = Char
type PairReactants = (Char,Char)
type Reaction = (PairReactants,Result)
type ReactionBook =  Reader [Reaction]
type Item = (Int,Char)


reactR :: PairReactants -> ReactionBook (Maybe Result)
reactR p = mapReader justTheResult ask
  where
    justTheResult :: [Reaction] -> Maybe Result
    justTheResult = head.filter (/= Nothing).map (`react`p)
    react :: Reaction -> PairReactants -> Maybe Result
    react (rcts1,r) rcts2 = if rcts1==rcts2 then Just r else Nothing

initialise :: Polymer -> [(Int, PairReactants)]
initialise p = mergeRecord $ map (\x -> (1,x))$ zip p (tail p)

mergeRecord:: [(Int, PairReactants)] -> [(Int, PairReactants)]
mergeRecord = foldr mergeEntry []
  where
    mergeEntry ::(Int, PairReactants) ->[(Int, PairReactants)] ->[(Int, PairReactants)]
    mergeEntry (n,rct) rcrd
      |(not$elem rct$snd<$>rcrd) = (n,rct):rcrd
      |otherwise           = let n1 = sum.map fst.filter ((==rct).snd)$ rcrd
                                 rest = filter ((/=rct).snd)$ rcrd
                             in (n1+n,rct):rest

reactRecord ::(Int, PairReactants) -> ReactionBook [(Int, PairReactants)]
reactRecord (n,(r0,r1)) = do rslt <- reactR (r0,r1)
                             case rslt of
                               Just r  -> return [(n,(r0,r)),(n,(r,r1))]
                               _       -> return [(n,(r0,r1))]

stepRecord ::[(Int, PairReactants)] -> ReactionBook[(Int, PairReactants)]
stepRecord rcrd = do newRcrds <- mapM reactRecord rcrd
                     return $ mergeRecord $ concat newRcrds

recordSummary ::[(Int, PairReactants)] -> [Item]
recordSummary [] = []
recordSummary ((n,(r1,r2)):rs) = (n,r1):recordSummary rs

polymerChainReaction :: Int -> Polymer -> ReactionBook [Item]
polymerChainReaction n polymer = do
  endRcrd <- foldl (>>=) (pure$initialise polymer) $ replicate n stepRecord
  return.mergeItems.recordSummary $ endRcrd

mergeItems :: [Item] -> [Item]
mergeItems = foldr mergeItem []
  where
    mergeItem :: Item -> [Item] -> [Item]
    mergeItem (n,c) rcrd
      |(not$elem c$snd<$>rcrd) = (n,c):rcrd
      |otherwise           = let n1 = fst.head.filter ((==c).snd)$ rcrd
                                 rest = filter ((/=c).snd)$ rcrd
                             in (n1+n,c):rest
{-============================================================================-}
answer2 :: IO [(Int,Char)]
answer2 = do
  (poly0, reactionBook0) <- inputIO
  let endState = sortWith (fst)$runReader (polymerChainReaction 40 poly0) reactionBook0
  putStrLn poly0 -- to manually add the last Char to its count in the endState....
  return$mergeItems$endState

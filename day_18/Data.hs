module Data where
import Parsing



filename = "data.txt"

data SnailNumber = Leaf Position Value | Node Position SnailNumber SnailNumber
  deriving (Show, Eq)
data Choice = L | R
   deriving (Show, Eq)
type Value = Int
type Position = [Choice] -- is finite

relabel :: SnailNumber -> SnailNumber
relabel s1 = relabelFrom [] s1
  where
    relabelFrom :: Position -> SnailNumber -> SnailNumber
    relabelFrom currPos (Leaf _ n)   = Leaf currPos n
    relabelFrom currPos (Node _ l r) = let lPos = currPos ++ [L]
                                           rPos = currPos ++ [R]
                                       in Node currPos (relabelFrom lPos l) (relabelFrom rPos r)

valueP :: Parser SnailNumber
valueP = do n <- nat
            return $ Leaf [] n

snailNumberP :: Parser SnailNumber
snailNumberP = do char '['
                  s1 <- valueP <|> snailNumberP
                  char ','
                  s2 <- valueP <|> snailNumberP
                  char ']'
                  return $ Node [] s1 s2

inputIO :: IO [SnailNumber]
inputIO = readFile filename >>= return.map (fst.head.parse snailNumberP).lines

testParse :: String -> SnailNumber
testParse = fst.head.parse snailNumberP

module Data13 where
import Parsing

filename = "data13.txt"

type Point = (Int,Int)
type Fold = (Char, Int)

inputIO :: IO ([Point], [Fold])
inputIO = do str <- readFile filename
             return.fst.head.parse ((,)<$>some positionP<*>some foldP) $ str

positionP :: Parser Point
positionP = do x <- nat
               token $ char ','
               y <- nat
               many $ char '\n'
               return (x,y)

foldP :: Parser Fold
foldP = do  some $ sat (not.(`elem`"xy"))
            xOry <- item
            char '='
            n <- nat
            many $ char '\n'
            return (xOry, n)

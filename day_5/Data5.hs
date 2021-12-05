module Data5 where
import Parsing

type Line = ((Int,Int),(Int,Int))

filename = "data5.txt"

inputIO :: IO [Line]
inputIO = readFile filename >>= return.fst.head.parse (some lineP)

lineP :: Parser Line
lineP = do tp1 <- tupleP
           token $ string "->"
           tp2 <- tupleP
           char '\n'
           return (tp1,tp2)

tupleP :: Parser (Int,Int)
tupleP = do nx <- nat
            char ','
            ny <- nat
            return (nx,ny)
{-=================-}
test = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2\n"

inputTest:: [Line]
inputTest = fst.head.parse (some lineP)$test

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

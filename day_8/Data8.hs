module Data8 where
import Parsing

filename = "data8.txt"
type Digit = String
type Problem = ([Digit],[Digit])

inputIO :: IO [Problem]
inputIO = readFile filename >>= return.fst.head.parse (some problemP)

problemP :: Parser Problem
problemP = do tenDgts <- some (token digitP)
              token$char '|'
              fourDgts <- some (token digitP)
              char '\n'
              return (tenDgts, fourDgts)

digitP :: Parser Digit
digitP = some letter

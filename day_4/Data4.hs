module Data4 where
import Parsing

type Table = [[Int]]

filename = "data4.txt"

inputIO :: IO ([Int],[Table])
inputIO = readFile filename >>= return.fst.head.parse inputP

inputP :: Parser ([Int], [Table])
inputP = do ns <- listP
            many $ char '\n'
            tbls <- some $ tableP <* (many $ char '\n')
            return (ns,tbls)

listP :: Parser [Int]
listP =  some $ nat <* (many $ char ',')

tableP :: Parser Table
tableP = some $ numLineP <* char '\n' 
 where
   numLineP :: Parser [Int]
   numLineP = some $ token nat

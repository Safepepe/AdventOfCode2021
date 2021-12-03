module Data3 where
import Parsing

filename = "data3.txt"

inputIO :: IO [[Int]]
inputIO = readFile filename >>= return.fst.head. parse (some binCodeP)

binCodeP :: Parser [Int]
binCodeP =  ((read.(:[]))<$>) <$> (some digit) <* char '\n'

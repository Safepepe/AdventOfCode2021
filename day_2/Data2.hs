module Data2 where
import Parsing

type Command = (String, Int)

filename = "data2.txt"

inputIO :: IO [Command]
inputIO = readFile filename >>= return. map (fst.head.parse commandP). lines

commandP :: Parser Command
commandP = do str <- token word
              n   <- token nat
              return (str,n)
  where
    word = some letter

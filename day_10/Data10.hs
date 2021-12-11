module Data10 where


filename = "data10.txt"

inputIO :: IO [String]
inputIO = readFile filename >>= return.lines

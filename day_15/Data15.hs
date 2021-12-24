module Data15 where

filename = "data15.txt"

inputIO :: IO [[Int]]
inputIO = readFile filename >>= return. map (map (read.(:[])) ).lines

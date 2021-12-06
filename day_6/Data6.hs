module Data6 where

filename = "data6.txt"

inputIO :: IO [Int]
inputIO = readFile filename >>= return.read

module Data1 where

filename = "data1.txt"

inputIO :: IO [Int]
inputIO = readFile filename >>= return.(map read).lines

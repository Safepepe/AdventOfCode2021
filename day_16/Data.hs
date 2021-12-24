module Data where

filename = "data.txt"

inputIO :: IO String
inputIO = readFile filename >>= return.init -- ignore the '\n' at the end

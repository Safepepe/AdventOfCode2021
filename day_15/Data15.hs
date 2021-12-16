module Data15 where

filename = "data15.txt"
type RiskMap = [[Int]]

inputIO :: IO RiskMap
inputIO = readFile filename >>= return. map (map (read.(:[])) ).lines 

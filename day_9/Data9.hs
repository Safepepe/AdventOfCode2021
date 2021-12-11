module Data9 where


filename = "data9.txt"

inputIO :: IO [[Int]]
inputIO = readFile filename >>= return.map (map (read.(:[]))).lines 

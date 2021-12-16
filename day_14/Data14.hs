module Data14 where
import Parsing

filename = "data14.txt"
type Polymer = String
type Reactants = (Char,Char)
type Result = Char
type Reaction = (Reactants,Result)

inputIO :: IO (Polymer, [Reaction])
inputIO = do str <- readFile filename
             return.fst.head.parse ((,)<$> polymerP <*> some reactionP)$str

polymerP :: Parser Polymer
polymerP = (some letter) <* (many$char '\n')

reactionP :: Parser Reaction
reactionP = do a <- letter
               b <- letter
               string " -> "
               result <- letter
               char '\n'
               return ((a,b), result)

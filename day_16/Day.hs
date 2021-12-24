module Day where
import Data (inputIO)
import Parsing
{-============================================================================-}
{-                             Part One                                       -}
{-============================================================================-}
type Packet = Either Literal Operator
type Literal = (Version, ID, Value)
data Operator = Operator (Version, ID , LengthTypeID, [Packet])
type Value = Int
type Version = Int
type ID = Int
type LengthTypeID = Int

toBits :: String -> String
toBits = concatMap toBit
  where
    toBit :: Char -> String
    toBit '0' = "0000"
    toBit '1' = "0001"
    toBit '2' = "0010"
    toBit '3' = "0011"
    toBit '4' = "0100"
    toBit '5' = "0101"
    toBit '6' = "0110"
    toBit '7' = "0111"
    toBit '8' = "1000"
    toBit '9' = "1001"
    toBit 'A' = "1010"
    toBit 'B' = "1011"
    toBit 'C' = "1100"
    toBit 'D' = "1101"
    toBit 'E' = "1110"
    toBit 'F' = "1111"
    toBit _   = error "not hexadecimal Char"


isLiteral :: Int -> Bool
isLiteral = (==4)

toDecimal :: String -> Int
toDecimal str = sum.zipWith (*) binList.reverse.take n$iterate (*2) 1
  where
     binList = map (read.(:[])) str
     n = length str

packetP :: Parser Packet
packetP =
  do v <- versionP
     t <- typeIdP
     if isLiteral t then
       do n <- literalP
          return $ Left (v,t,n)
     else
       do l <- lengthTypeIdP
          if l == 0 then
            do kBits <- bitsP 15
               str <- takeBitsP kBits
               let subPackets = fst.head$parse (many packetP) str
               return $ Right$Operator (v,t,l,subPackets)
          else
            do nSubs <- bitsP 11
               subPackets <- subPacketsP nSubs
               return $ Right$Operator (v, t, l, subPackets)

versionP :: Parser Version
versionP = do d0 <- digit
              d1 <- digit
              d2 <- digit
              return $ toDecimal [d0,d1,d2]

typeIdP :: Parser ID
typeIdP = do d0 <- digit
             d1 <- digit
             d2 <- digit
             return $ toDecimal [d0,d1,d2]

literalP :: Parser Value
literalP = do initFiveBitGroups <- many initFiveBits
              lastBits <- lastFiveBits
              return $ toDecimal $ foldr (++) lastBits initFiveBitGroups
  where
    lastFiveBits :: Parser String
    lastFiveBits = do char '0'
                      d1 <- digit
                      d2 <- digit
                      d3 <- digit
                      d4 <- digit
                      return $ [d1,d2,d3,d4]
    initFiveBits :: Parser String
    initFiveBits = do char '1'
                      d1 <- digit
                      d2 <- digit
                      d3 <- digit
                      d4 <- digit
                      return $ [d1,d2,d3,d4]

bitsP :: Int -> Parser Int
bitsP n = sequence (replicate n digit) >>= return.toDecimal

takeBitsP :: Int -> Parser String
takeBitsP n =  sequence (replicate n digit)

subPacketsP :: Int -> Parser [Packet]
subPacketsP n = sequence (replicate n packetP)

lengthTypeIdP :: Parser Int
lengthTypeIdP = digit >>= return.read.(:[])

versionNumbers :: Packet -> [Int]
versionNumbers (Left (v,_,_))          = [v]
versionNumbers (Right (Operator (v,_,_,sbpckts))) =  v:(concat$versionNumbers<$>sbpckts)
{-============================================================================-}
answer1 :: IO Int
answer1 = do hexas <- inputIO
             let binStr = toBits hexas
                 pcket = fst.head$parse packetP binStr
                 vns = sum$versionNumbers pcket
             return vns
{-============================================================================-}
{-                             Part Two                                       -}
{-============================================================================-}
eval :: Packet -> Int
eval (Left (_,_,val)) = val
eval (Right(Operator (_,idx,_,sbpckts)))
   | idx == 0 = sum$ eval<$> sbpckts
   | idx == 1 = product$ eval<$> sbpckts
   | idx == 2 = foldr1 min $ eval<$>sbpckts
   | idx == 3 = foldr1 max $ eval<$>sbpckts
   | idx == 5 = if eval (sbpckts!!0) > eval (sbpckts!!1) then 1 else 0
   | idx == 6 = if eval (sbpckts!!0) < eval (sbpckts!!1) then 1 else 0
   | idx == 7 = if eval (sbpckts!!0) == eval (sbpckts!!1) then 1 else 0
{-============================================================================-}
answer2 :: IO Int
answer2 = do hexas <- inputIO
             let binStr = toBits hexas
                 pcket = fst.head$parse packetP binStr
             return$ eval pcket

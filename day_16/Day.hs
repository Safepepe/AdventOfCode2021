module Day where
import Data (inputIO)
import Parsing
{-============================================================================-}
{-                             Part One                                       -}
{-============================================================================-}
type Packet = Either Literal Operator
type Literal = (Version, ID, Value)
type Operator = (Version, ID , LengthTypeID, [Packet])
type Value = Int
type Version = Int
type ID = Int
type LengthTypeID = Int

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

toBits :: String -> String
toBits = concatMap toBit

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
               return $ Right (v,t,l,subPackets)
          else
            do nSubs <- bitsP 11
               subPackets <- subPacketsP nSubs
               return $ Right (v, t, l, subPackets)

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
literalP = do d0 <- digit
              if d0 == '0' then
                d1 <- digit
                d2 <- digit
                d3 <- digit
                d4 <- digit
                
bitsP :: Int -> Parser Int
takeBitsP :: Int -> Parser String
subPacketsP :: Int -> Parser [Packet]


{-============================================================================-}

{-============================================================================-}
{-                             Part One                                       -}
{-============================================================================-}

{-============================================================================-}

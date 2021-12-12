module Day8 where
import Data8 (inputIO)
import Data.List (intersect, (\\))
{-============================================================================-}
{-                             Part One                                       -}
{-============================================================================-}
type Digit = String
type Problem = ([Digit],[Digit])
type LeftPart = [Digit]

easyDigit :: Digit -> Bool
easyDigit d = length d `elem` [2,3,4,7]

countEasyDigits :: Problem -> Int
countEasyDigits = length.filter easyDigit.snd
{-============================================================================-}
answer1:: IO Int
answer1 = do pbs <- inputIO
             return.sum$countEasyDigits<$>pbs
{-============================================================================-}
{-                             Part Two                                       -}
{-============================================================================-}
subset :: String -> String -> Bool
subset str1 str2 = length (str2\\str1) == (length str2 - length str1)

isThree :: LeftPart -> Digit -> Bool
isThree tenDs d = length d == 5 && 2==differences
  where
    differences = sum$(length.(d\\))<$>others
    others = filter (\x -> x/=d && length x ==5) tenDs

isOne :: LeftPart -> Digit -> Bool
isOne _ d = length d == 2

isSeven :: LeftPart -> Digit -> Bool
isSeven _ d = length d == 3

isFour :: LeftPart -> Digit -> Bool
isFour _ d = length d == 4

isEight :: LeftPart -> Digit -> Bool
isEight _ d = length d == 7

letterA ::  LeftPart -> Char
letterA tenDgs = head$seven\\one
  where
    seven = head.filter (isSeven [])$tenDgs
    one   = head.filter (isOne  [])$tenDgs

isFive :: LeftPart -> Digit -> Bool
isFive tenDgs d = length d == 5 && differences234 == 4
  where
    others = map (\\[a]).filter (\x -> (x/=d && length x ==5) || length x == 4)$tenDgs
    a = letterA tenDgs
    d0 = filter (/= a) d
    differences234 = sum.map length$(d0\\)<$> others

isTwo :: LeftPart -> Digit -> Bool
isTwo pb d = length d == 5 && (not.isFive pb$d) && (not.isThree pb$d)

isNine :: LeftPart -> Digit -> Bool
isNine pb d = length d == 6 && three`subset`d
  where
    three = head.filter (isThree pb)$pb

isZero :: LeftPart -> Digit -> Bool
isZero pb d = length d == 6 && one`subset`d && (not.isNine pb$d)
  where
    one = head.filter (isOne pb)$pb

isSix :: LeftPart -> Digit -> Bool
isSix pb d = length d == 6 && (not$one`subset`d)
  where
    one = head.filter (isOne pb)$pb

solve :: Problem -> Int
solve (tenDgs, fourDgs) = foldl1 (\x y -> x*10 + y)$(toNumber tenDgs)<$>fourDgs

toNumber :: LeftPart -> Digit -> Int
toNumber ds d
  |isZero ds d  = 0
  |isOne ds d   = 1
  |isTwo ds d   = 2
  |isThree ds d = 3
  |isFour ds d  = 4
  |isFive ds d  = 5
  |isSix ds d   = 6
  |isSeven ds d = 7
  |isEight ds d = 8
  |isNine ds d  = 9
{-============================================================================-}
answer2 :: IO Int
answer2 = do pbs <- inputIO
             return.sum$solve<$>pbs

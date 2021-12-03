module Day2 where
import Data2 (inputIO)

{-============================================================================-}
{-+                                Part One                                  +-}
{-============================================================================-}
type Command = (String, Int)
type Position = (Int, Int)

initPos = (0, 0) -- (x,y); y = depth

step :: Command -> Position -> Position
step ("forward", n) (x,y) = (x + n, y)
step ("up", n)      (x,y) = (x, y - n)
step ("down", n)    (x,y) = (x, y + n)
step  _             (x,y) = (x, y)
{-====================================================-}
answer1 :: IO Position
answer1 = do cmds <- inputIO
             let rCmds = reverse cmds
             return $ foldr step initPos rCmds
{-============================================================================-}
{-+                                Part Two                                  +-}
{-============================================================================-}
type Aim = Int
type Submarine = (Position, Aim)

initSub = (initPos,0)

step2 :: Command -> Submarine -> Submarine
step2 ("forward", n) ((x,y),a) = ((x+n,y+a*n),a)
step2 ("up", n)      ((x,y),a) = ((x,y),a-n)
step2 ("down", n)    ((x,y),a) = ((x,y),a+n)
step2  _             ((x,y),a) = ((x,y),a)

answer2 :: IO Submarine
answer2 = do cmds <- inputIO
             let rCmds = reverse cmds
             return $ foldr step2 initSub rCmds

module Day where
{-============================================================================-}
{-                             Part One                                       -}
{-============================================================================-}
type TargetArea = (XCoords, YCoords)
type XCoords = (Int,Int)
type YCoords = (Int,Int)
type Position = (Int,Int)
type Velocity = (Int,Int)
type Time = Int
type YCoord = Int
type XCoord = Int
type YVelocity = Int
type XVelocity = Int

input :: TargetArea
input = ((94,151),(-156,-103))

yPosition :: YVelocity -> Time -> YCoord
yPosition v0 n = n*v0 - (n*n - n)`div`2

{-============================================================================-}
answer1 :: IO Int
answer1 = return 155 --this was done on paper ...
{-============================================================================-}
{-                             Part Two                                       -}
{-============================================================================-}
xPosition :: XVelocity -> Time -> XCoord
xPosition v0 n
  | n > abs(v0) = v0*v0 - signum(v0)*((v0*v0 - abs(v0))`div`2)
  | otherwise   = n*v0 - signum(v0)*((n*n - n)`div`2)

inTargetArea :: TargetArea -> Position -> Bool
inTargetArea ((xmin,xmax),(ymin,ymax)) (x,y) = x >= xmin && y >= ymin && x <= xmax && y <= ymax

entersTargetArea :: TargetArea -> (XVelocity, YVelocity) -> Bool
entersTargetArea tArea (vx,vy)
  | vy <= 0  = or$(inTargetArea tArea.position (vx,vy))<$>[0..(vy + 170)]
  | vy > 0   = or$(inTargetArea tArea.position (vx,vy))<$>[(2*vy+1)..(vy+170)]

position :: (XVelocity, YVelocity) -> Time -> (XCoord,YCoord)
position (vx,vy) n = (xPosition vx n, yPosition vy n)

yVmin :: YVelocity
yVmin = -156
yVmax :: YVelocity
yVmax = 155
xVmax :: XVelocity
xVmax = 151
xVmin :: XVelocity
xVmin = 12
potentialVelocities :: [(XVelocity,YVelocity)]
potentialVelocities = [(vx,vy) | vx <- [xVmin..xVmax], vy <- [yVmin..yVmax]]
{-============================================================================-}
answer2 :: Int
answer2 = length$ filter (entersTargetArea input) potentialVelocities

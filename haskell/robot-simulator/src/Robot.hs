{-# LANGUAGE DeriveAnyClass  #-}
module Robot (Bearing(..), Robot, mkRobot,
              coordinates, simulate,
              bearing, turnRight, turnLeft) where

type Coordinates = (Integer, Integer)
type Instruction = Char
type Instructions = [Instruction]

class (Enum a, Bounded a, Eq a) => Circular a where
  previous :: a -> a
  previous x
    | x == minBound = maxBound
    | otherwise = pred x

  next :: a -> a
  next x
    | x == maxBound = minBound
    | otherwise = succ x

data Bearing = North | East | South | West
  deriving (Eq, Show, Bounded, Enum, Circular)

data Robot = Robot { bearing :: Bearing, coordinates :: Coordinates }
  deriving (Eq, Show)

simulate :: Robot -> Instructions -> Robot
simulate = foldl execute

execute :: Robot -> Instruction -> Robot
execute (Robot b c@(x, y)) instruction =
  case instruction of
    'L' -> mkRobot (turnLeft b) c
    'R' -> mkRobot (turnRight b) c
    'A' -> mkRobot b advance
    i   -> error $ "invalid instruction: " ++ [i]
  where
    advance :: Coordinates
    advance =
      case b of
        South -> (x, y-1)
        North -> (x, y+1)
        West  -> (x-1, y)
        East  -> (x+1, y)

mkRobot :: Bearing -> Coordinates -> Robot
mkRobot = Robot

turnRight :: Bearing -> Bearing
turnRight = next

turnLeft :: Bearing -> Bearing
turnLeft = previous

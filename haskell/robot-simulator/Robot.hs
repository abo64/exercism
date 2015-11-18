module Robot (Bearing(..), Robot, mkRobot,
              coordinates, simulate,
              bearing, turnRight, turnLeft) where

type Coordinates = (Int, Int)
type Instruction = Char
type Instructions = [Instruction]

data Bearing = North | West | South | East
  deriving (Eq, Show)

data Robot = Robot Bearing Coordinates
  deriving (Eq, Show)

simulate :: Robot -> Instructions -> Robot
simulate = foldl execute

execute :: Robot -> Instruction -> Robot
execute (Robot b c@(x, y)) instruction =
  case instruction of
    'L' -> mkRobot (turnLeft b) c
    'R' -> mkRobot (turnRight b) c
    'A' -> mkRobot b advance
    i -> error $ "invalid instruction: " ++ [i]
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

coordinates :: Robot -> Coordinates
coordinates (Robot _ c) = c

bearing :: Robot -> Bearing
bearing (Robot b _) = b

turnRight :: Bearing -> Bearing
turnRight b = case b of
  North -> East; East -> South; South -> West; West -> North

turnLeft :: Bearing -> Bearing
turnLeft b = case b of
  North -> West; East -> North; South -> East; West -> South
  
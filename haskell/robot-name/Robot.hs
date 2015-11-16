module Robot (robotName, mkRobot, resetName) where

import System.Random (newStdGen, randomRs)
import Data.IORef

type Robot = IORef String

robotName :: Robot -> IO String
robotName = readIORef

mkRobot :: IO Robot
mkRobot = do
  name <- newName
  newIORef name

resetName :: Robot -> IO ()
resetName robot = do
  name <- newName
  writeIORef robot name

newName :: IO String
newName = do
  gen <- newStdGen
  let randomLetters = randomRs ('A', 'Z') gen
      randomDigits = randomRs ('0', '9') gen
      letters = take 2 randomLetters
      digits = take 3 randomDigits
      name = letters ++ digits
  return name
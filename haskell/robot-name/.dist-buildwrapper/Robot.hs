module Robot (robotName, mkRobot, resetName) where

import System.Random (newStdGen, randomRs)
import Data.IORef

newtype Robot = Robot { nameRef :: IORef String }

robotName :: Robot -> IO String
robotName robot =
  readIORef (nameRef robot)

mkRobot :: IO Robot
mkRobot = do
  name <- newName
  nameR <- newIORef name
  return (Robot nameR)

resetName :: Robot -> IO ()
resetName robot = do
  name <- newName
  writeIORef (nameRef robot) name

newName :: IO String
newName = do
  gen <- newStdGen
  let randomLetters = randomRs ('A', 'Z') gen
      randomDigits = randomRs ('0', '9') gen
      letters = take 2 randomLetters
      digits = take 3 randomDigits
      name = letters ++ digits
  return name
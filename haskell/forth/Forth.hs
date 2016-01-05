{-# LANGUAGE OverloadedStrings #-}
module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , formatStack
  , empty
  ) where

import Data.Text (Text, split, pack, unpack, toUpper)
import Data.Maybe (isJust, fromJust)
import Data.Char (isAlphaNum, isSymbol)

type ParsedInput = [String]
type ForthResult = Either ForthError ForthState

type WordName = String
type WordDefinition = [String]

data ForthState = ForthState { stack::[Int], newWords::[(WordName, WordDefinition)] }

empty :: ForthState
empty = ForthState [] []

push :: Int -> ForthState -> ForthState
push x (ForthState xs nws) = ForthState (x:xs) nws

pop :: ForthState -> Either ForthError (Int, ForthState)
pop (ForthState (x:xs) nws) = Right (x, ForthState xs nws)
pop _ = Left StackUnderflow

lookupWordDefinition :: WordName -> ForthState -> Maybe WordDefinition
lookupWordDefinition w = lookup w . newWords

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

-- | Evaluate an input Text, returning the new state
evalText :: Text -> ForthState -> ForthResult
evalText t s = evalForth (parseInput t) (Right s)

-- foldl over ParsedInput would be more natural,
-- but I had no better idea to account for new word definitions
evalForth :: ParsedInput -> ForthResult -> ForthResult
evalForth [] s = s
evalForth (":":wordName:xs) forthResult =
  evalForth parsedInputRest newForthResult
  where
    newForthResult = do
      (ForthState s nws) <- forthResult
      wd <- newWordDef
      return (ForthState s (wd:nws))

    newWordDef :: Either ForthError (WordName, WordDefinition)
    newWordDef = if isJust $ parseNumber wordName
      then Left InvalidWord
      else Right (upperCaseWordName, wordDef)

    upperCaseWordName = toUpperCase wordName
    (wordDef, parsedInputRest) = getWordDef [] xs

    getWordDef :: [String] -> ParsedInput -> (WordDefinition, ParsedInput)
    getWordDef wd (";":ys) = (reverse wd, ys)
    getWordDef wd (y:ys) = getWordDef (y:wd) ys
evalForth (x:xs) s = evalForth nextParsedInput nextForthResult
  where
    nextForthResult
      | isJust maybeWordDef = s
      | x == "+" = s >>= doBinaryOp (+)
      | x == "-" = s >>= doBinaryOp (-)
      | x == "*" = s >>= doBinaryOp (*)
      | x == "/" = s >>= doDiv
      | upperCaseWord == "DUP" = s >>= doDup
      | upperCaseWord == "DROP" = s >>= doDrop
      | upperCaseWord == "SWAP" = s >>= doSwap
      | upperCaseWord == "OVER" = s >>= doOver
      | isJust maybeNumber = s >>= doPushNumber (fromJust maybeNumber)
      | otherwise = Left $ UnknownWord $ pack x
    nextParsedInput = maybe xs (++ xs) maybeWordDef
    maybeWordDef = either (const Nothing) (lookupWordDefinition upperCaseWord) s
    maybeNumber = parseNumber x
    upperCaseWord = toUpperCase x

doBinaryOp :: (Int -> Int -> Int) -> ForthState -> ForthResult
doBinaryOp op s = do
  (y, s1) <- pop s
  (z, s2) <- pop s1
  Right (push (z `op` y) s2)

doDiv :: ForthState -> ForthResult
doDiv s = do
  (y, s1) <- pop s
  (z, s2) <- pop s1
  if y /= 0 then Right (push (z `div` y) s2)
            else Left DivisionByZero

doDup :: ForthState -> ForthResult
doDup s = do
  (y, s1) <- pop s
  s2 <- Right (push y s1)
  Right (push y s2)

doDrop :: ForthState -> ForthResult
doDrop s = do
  (_, s1) <- pop s
  Right s1

doSwap :: ForthState -> ForthResult
doSwap s = do
  (y, s1) <- pop s
  (z, s2) <- pop s1
  s3 <- Right (push y s2)
  Right (push z s3)

doOver :: ForthState -> ForthResult
doOver s = do
  (y, s1) <- pop s
  (z, s2) <- pop s1
  s3 <- Right (push z s2)
  s4 <- Right (push y s3)
  Right (push z s4)

doPushNumber :: Int -> ForthState -> ForthResult
doPushNumber n s = Right (push n s)

toUpperCase :: String -> String
toUpperCase = unpack . toUpper . pack

parseNumber :: String -> Maybe Int
parseNumber str = case reads str :: [(Int,String)] of [(n,"")] -> Just n
                                                      _ -> Nothing

parseInput :: Text -> ParsedInput
parseInput = map unpack . split notWordChar
  where
-- did not want to dive into regex matching, so I tried to keep it simple
    notWordChar c = not (isAlphaNum c || isSymbol c || c `elem` "+-/*:;")

-- | Return the current stack as Text with the element
-- on top of the stack being the rightmost element in the output
formatStack :: ForthState -> Text
formatStack = pack . unwords . map show . reverse . stack

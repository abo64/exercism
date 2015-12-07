module WordProblem (answer) where

import Text.Regex.Posix

type Question = String
type Answer = Maybe Int
type Expression = [String]

answer :: Question -> Answer
answer question =
  parseExpression question >>= (fst . foldl reduce (Just 0, (+))) 

parseExpression :: Question -> Maybe Expression
parseExpression question =
  fmap (filter (/= "by") . words) exprStr
  where
    exprStr = safeHead matchList
    (_,_,_,matchList) = question =~ "What is (.+)\\?" :: (String,String,String,[String])

    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x:_) = Just x


reduce :: (Maybe Int, Int -> Int -> Int) -> String -> (Maybe Int, Int -> Int -> Int)
reduce (result, operation) next = case next of
  "multiplied" -> (result, (*))
  "divided" -> (result, quot)
  "plus" -> (result, (+))
  "minus" -> (result, (-))
  str -> (newResult str, operation)
  where
    newResult str = parseNumber str >>= applyOperation
    applyOperation number = fmap (`operation` number) result

    parseNumber :: String -> Maybe Int
    parseNumber str =
      if isParseableNumber str then Just $ read str else Nothing
      where
        isParseableNumber :: String -> Bool
        isParseableNumber = (=~ "-?[0-9]+")

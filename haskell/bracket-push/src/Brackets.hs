module Brackets (arePaired) where

import qualified Data.Map as M
import Data.Tuple (swap)
import Data.Maybe (isNothing)
import Data.List (uncons)

arePaired :: String -> Bool
arePaired = (emptyStack ==) . foldl matchBrackets emptyStack
  where
    matchBrackets stack char
      | isLeftBracket char = push char stack
      | isNothing $ leftBracket char = stack
      | Just x <- leftBracket char
      , Just (y, xs) <- pop stack
      , x == y
      = Just xs
      | otherwise = Nothing

    brackets = M.fromList $ swap <$> [('[', ']'), ('(', ')'), ('{', '}')]
    isLeftBracket = flip elem (M.elems brackets)
    leftBracket = flip M.lookup brackets

    emptyStack = Just [] :: Maybe [Char]
    push x = ((x :) <$>)
    pop = (uncons =<<)

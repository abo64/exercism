{-# LANGUAGE TupleSections #-}
module Dominoes (chain) where

import Data.List (permutations, foldl', find)
import Data.Maybe (isJust)
import Data.Tuple (swap)
import Control.Monad (join, guard)

type Domino = (Int, Int)
type Dominoes = [Domino]

chain :: Dominoes -> Maybe Dominoes
chain dominoes = join $ find isJust solutions
  where
    solutions :: [Maybe Dominoes]
    solutions = chain' <$> permutations dominoes

chain' :: Dominoes -> Maybe Dominoes
chain' [] = Just []
chain' ys@(x:xs) = do
  (rightDomino, leftDomino) <- matchingDominoes (last ys) x
  let startChain = Just [leftDomino]
  result <- foldl' appendDomino startChain xs
  guard $ last result == rightDomino
  return result
  where
    appendDomino :: Maybe [Domino] -> Domino -> Maybe [Domino]
    appendDomino mxs candidate = do
      chainSoFar <- mxs
      let lastDomino = last chainSoFar
      nextDomino <- matchingDomino lastDomino candidate
      return $ chainSoFar ++ [nextDomino]

    matchingDomino :: Domino -> Domino -> Maybe Domino
    matchingDomino (_, y1) (x2, y2)
      | y1 == x2 = Just (x2, y2)
      | y1 == y2 = Just (y2, x2)
      | otherwise = Nothing

    matchingDominoes :: Domino -> Domino -> Maybe (Domino, Domino)
    matchingDominoes x' y'
      | isJust y1 = (x',) <$> y1
      | otherwise = (x1,) <$> matchingDomino x1 y'
      where
        y1 = matchingDomino x' y'
        x1 = swap x'

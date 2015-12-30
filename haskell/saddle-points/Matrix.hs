module Matrix (saddlePoints) where

import Data.Array

type Coordinate = (Integer, Integer)
type Element = Integer
type Points = [Coordinate]
type Row = [Element]
type Col = [Element]
type MatrixRepr = Array Coordinate Element

saddlePoints :: MatrixRepr -> Points
saddlePoints matrix = filter isSaddlePoint coordinates
  where
    isSaddlePoint :: Coordinate -> Bool
    isSaddlePoint coordinate = element == rowMax && element == colMin
      where
        element = getElement coordinate
        rowMax = maximum $ getRow coordinate
        colMin = minimum $ getCol coordinate
  
    coordinates :: [Coordinate]
    coordinates = indices matrix

    getElement :: Coordinate -> Element
    getElement = (matrix !)

    getRow :: Coordinate -> Row
    getRow = getRelatedElements fst

    getCol :: Coordinate -> Col
    getCol = getRelatedElements snd

    getRelatedElements :: (Coordinate -> Integer) -> Coordinate -> [Element]
    getRelatedElements extractor coordinate = map getElement relatedCoordinates
      where relatedCoordinates = filter ((== extractor coordinate) . extractor) coordinates

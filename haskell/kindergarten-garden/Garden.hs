module Garden (garden, defaultGarden, lookupPlants, Plant(..)) where

import qualified Data.Map.Strict as Map
import Data.List (sort)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Control.Applicative ((<$>))

data Plant = Violets | Clover | Radishes | Grass
  deriving (Eq, Show)

type Cup = Char
type Child = String
type Children = [Child]
type Plants = [Plant]
type Garden = Map.Map Child Plants
type GardenLayout = String

garden :: Children -> GardenLayout -> Garden
garden children gardenLayout =
  Map.fromList $ zip sortedChildren plantsPerChild
  where
    sortedChildren :: Children
    sortedChildren = sort children
    plantsPerChild :: [Plants]
    plantsPerChild = zipWith (++) (head plantRowsPerChild) (head $ tail plantRowsPerChild)
    plantRowsPerChild :: [[Plants]]
    plantRowsPerChild = map (chunksOf 2) plantRows
    plantRows :: [Plants]
    plantRows = map cupsToPlants $ lines gardenLayout
    cupsToPlants :: [Cup] -> Plants
    cupsToPlants = map $ fromJust . flip Map.lookup cupToPlant

defaultGarden :: GardenLayout -> Garden
defaultGarden = garden defaultChildren

lookupPlants :: Child -> Garden -> Plants
lookupPlants child = fromJust <$> Map.lookup child

defaultChildren :: Children
defaultChildren =
  ["Alice", "Bob", "Charlie", "David", "Eve", "Fred"
  ,"Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]
  
cupToPlant :: Map.Map Cup Plant
cupToPlant =
  Map.fromList [('V', Violets), ('C', Clover), ('R', Radishes), ('G', Grass)]
module School (School, add, empty, sorted, grade) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.List (sort)
import Data.Maybe (fromMaybe)

type Grade = Int
type Name = String
type School = Map Grade [Name]

add :: Grade -> Name -> School -> School
add grad name =
  M.insertWith (++) grad [name]

empty :: School
empty = M.empty

sorted :: School -> [(Grade, [Name])]
sorted school =
  M.toList withSortedValues
  where
    withSortedValues = M.map sort school

grade :: Grade -> School -> [Name]
grade grad school =
  fromMaybe [] (M.lookup grad school)

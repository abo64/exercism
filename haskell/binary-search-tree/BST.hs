module BST (bstLeft, bstRight, bstValue, singleton, insert, fromList, toList) where

import Data.Maybe
import Control.Applicative ((<$>))

type BstValue = Int

type BstNode = Maybe Bst
data Bst = Bst { left :: BstNode, right :: BstNode, value :: BstValue }

bstLeft :: Bst -> BstNode
bstLeft = left

bstRight :: Bst -> BstNode
bstRight = right

bstValue :: Bst -> BstValue
bstValue = value

singleton :: BstValue -> Bst
singleton = Bst Nothing Nothing

insert :: BstValue -> Bst -> Bst
insert newValue (Bst l r v)
  | newValue <= v = insertLeft
  | otherwise = insertRight
  where
    insertLeft = Bst (insertInto l) r v
    insertRight = Bst l (insertInto r) v

    insertInto :: BstNode -> BstNode
    insertInto targetNode = Just $ fromMaybe newBst insertedNode
      where
        insertedNode :: BstNode
        insertedNode = insert newValue <$> targetNode
        newBst = singleton newValue

fromList :: [BstValue] -> Bst
fromList [] = error "fromList called on empty list"
fromList (x:xs) =
  foldl (flip insert) (singleton x) xs

toList :: Bst -> [BstValue]
toList (Bst l r v) = leftList ++ [v] ++ rightList
  where
    leftList = nodeList l
    rightList = nodeList r
    nodeList = maybe [] toList 

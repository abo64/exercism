module Zipper (
    BinTree(..),
    Zipper,

    fromTree,
    toTree,

    value,
    left,
    right,
    up,

    setValue,
    setLeft,
    setRight
) where

import Data.Maybe (fromJust)

-- | A binary tree.
data BinTree a = BT { 
    btValue :: a                 -- ^ Value
  , btLeft  :: Maybe (BinTree a) -- ^ Left child
  , btRight :: Maybe (BinTree a) -- ^ Right child
} deriving (Eq, Show)

data Crumb a = LeftCrumb a (Maybe (BinTree a)) | RightCrumb a (Maybe (BinTree a)) deriving (Eq, Show)
type Breadcrumbs a = [Crumb a]

-- | A zipper for a binary tree.
type Zipper a = (BinTree a, Breadcrumbs a) -- deriving (Eq, Show)

-- | Get a zipper focussed on the root node.
fromTree :: BinTree a -> Zipper a
fromTree bt = (bt, [])

-- | Get the complete tree from a zipper.
toTree :: Zipper a -> BinTree a
toTree (bt, []) = bt
toTree z = toTree $ fromJust (up z)

-- | Get the value of the focus node.
value :: Zipper a -> a
value (BT x _ _, _) = x

-- | Get the left child of the focus node, if any.
left :: Zipper a -> Maybe (Zipper a)
left (BT x (Just l) r, bs) = Just (l, LeftCrumb x r:bs)
left _ = Nothing

-- | Get the right child of the focus node, if any.
right :: Zipper a -> Maybe (Zipper a)
right (BT x l (Just r), bs) = Just (r, RightCrumb x l:bs)
right _ = Nothing

-- | Get the parent of the focus node, if any.
up :: Zipper a -> Maybe (Zipper a)
up (t, LeftCrumb x r:bs) = Just (BT x (Just t) r, bs)
up (t, RightCrumb x l:bs) = Just (BT x l (Just t), bs)
up (_, []) = error "up: called on topmost focus"

-- | Set the value of the focus node.
setValue :: a -> Zipper a -> Zipper a
setValue v (BT _ l r, bs)  = (BT v l r, bs)

-- | Replace a left child tree.
setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft t (BT x _ r, bs) = (BT x t r, bs)

-- | Replace a right child tree.
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight t (BT x l _, bs) = (BT x l t, bs)

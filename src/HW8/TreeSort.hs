module HW8.TreeSort(treeSort) where

import Data.Monoid

data Tree a = Leaf | Node (Tree a) a (Tree a)

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x (Node t y t') = case compare x y of
    GT  ->  Node t y (insert x t')
    _   ->  Node (insert x t) y t'

flatten :: Tree a -> [a]
flatten Leaf = []
flatten (Node t x t') = flatten t <> [x] <> flatten t'

treeSort :: Ord a => [a] -> [a]
treeSort = flatten . foldr insert Leaf


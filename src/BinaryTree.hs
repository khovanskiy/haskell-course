{-# LANGUAGE FlexibleInstances #-}
module BinaryTree(Tree, fromList, insert, find, delete, toList, next) where

import Set

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)

helper :: (Ord a) => Tree a -> (a, Tree a)
helper (Node m Leaf hr) = (m, hr)
helper (Node m hl hr) = do
    let (key, subTree) = helper hl
    (key, (Node m subTree hr))

deleteRoot :: (Ord a) => Tree a -> Tree a
deleteRoot (Node _ Leaf right) = right
deleteRoot (Node _ left Leaf) = left
deleteRoot (Node _ left (Node y Leaf right)) = (Node y left right)
deleteRoot (Node _ left right) = (Node new_key left new_sub) where
    (new_key, new_sub) = helper right

instance Set Tree where
    emptySet = Leaf

    toList Leaf        = []
    toList (Node x l r) = toList l ++ [x] ++ toList r

    insert Leaf m = (Node m Leaf Leaf)
    insert (Node k left right) m = case compare m k of
        EQ  -> Node m left right
        LT  -> Node k (insert left m) right
        _   -> Node k left (insert right m)

    find Leaf _ = Nothing
    find (Node k left right) m = case compare m k of
        EQ  -> Just k
        LT  -> find left m
        _   -> find right m

    delete Leaf _ = Leaf
    delete (Node k left right) m = case compare m k of
        LT  -> Node k left (delete right m)
        GT  -> Node k (delete left m) right
        _   -> deleteRoot (Node k left right)

    next Leaf _ = Nothing
    next (Node x left right) y
        | y < x     = if lres == Nothing then Just x else lres
        | otherwise = next right y
        where
            lres = next left y
--printTree :: (Ord a, Show a) => Tree a -> IO ()
--printTree = mapM_ putStrLn . treeIndent
--  where
--    treeIndent Leaf           = ["-- /-"]
--    treeIndent (Node k lb rb) =
--      ["--" ++ show k] ++
--      map ("  |" ++) ls ++
--      ("  `" ++ r) : map ("   " ++) rs
--      where
--        (r:rs) = treeIndent rb
--        ls     = treeIndent lb

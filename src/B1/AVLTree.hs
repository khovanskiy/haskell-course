module B1.AVLTree(Tree(..), printTree, isBalanced) where

import Set

data Tree a = Leaf | Node {key :: a, hh :: Int, left :: Tree a, right :: Tree a} deriving (Show)

cached_height :: Tree a -> Int
cached_height Leaf = 0
cached_height tree = hh tree

bfactor :: Tree a -> Int
bfactor tree = (cached_height $ right tree) - (cached_height $ left tree)

height :: Tree a -> Tree a -> Int
height l r = (max hl hr) + 1 where
    hl = cached_height l
    hr = cached_height r

create :: a -> Tree a -> Tree a -> Tree a
create k l r = Node k (height l r) l r

rotate_right :: Tree a -> Tree a
rotate_right (Node p _ (Node q _ a b) c) = create q a newP where
    newP = Node p (height b c) b c

rotate_left :: Tree a -> Tree a
rotate_left (Node q _ a (Node p _ b c)) = create p newQ c where
    newQ = Node q (height a b) a b

balance :: Tree a -> Tree a
balance tree@(Node k h l r) | bfactor tree == 2 = do
    if (bfactor $ right tree) < 0
        then rotate_left $ Node k h l (rotate_right r)
        else rotate_left tree
balance tree@(Node k h l r) | bfactor tree == -2   = do
    if (bfactor $ left tree) > 0
        then rotate_right $ Node k h (rotate_left l) r
        else rotate_right tree
balance tree = tree

helper :: (Ord a) => Tree a -> (a, Tree a)
helper (Node m _ Leaf r) = (m, r)
helper (Node m _ l r) = (newK, balance $ create m newL r) where
    (newK, newL) = helper l

deleteRoot :: (Ord a) => Tree a -> Tree a
deleteRoot (Node _ _ Leaf r) = r
deleteRoot (Node _ _ l Leaf) = l
deleteRoot (Node _ _ l (Node y _ Leaf r)) = balance $ create y l r
deleteRoot (Node _ _ l r) = balance $ create newK l newR where
    (newK, newR) = helper r

isBalanced :: Tree a -> Bool
isBalanced = (<2) . bfactor

printTree :: (Ord a, Show a) => Tree a -> IO ()
printTree = mapM_ putStrLn . treeIndent where
    treeIndent Leaf           = ["-- /-"]
    treeIndent (Node k _ lb rb) =
      ["--" ++ show k] ++
      map ("  |" ++) ls ++
      ("  `" ++ r) : map ("   " ++) rs
      where
        (r:rs) = treeIndent rb
        ls     = treeIndent lb

instance Set Tree where
    emptySet = Leaf

    toList Leaf        = []
    toList (Node x _ l r) = toList l ++ [x] ++ toList r

    find Leaf _ = Nothing
    find (Node k _ l r) m = case compare m k of
        EQ  -> Just k
        LT  -> find l m
        _   -> find r m

    insert Leaf m = Node m 0 Leaf Leaf
    insert (Node k h l r) m = case compare m k of
            EQ  -> Node m h l r
            LT  -> balance $ create k (insert l m) r
            _   -> balance $ create k l (insert r m)

    delete Leaf _ = Leaf
    delete (Node k h l r) m = case compare m k of
        GT  -> balance $ create k l (delete r m) where
        LT  -> balance $ create k (delete l m) r
        _   -> deleteRoot (Node k h l r)
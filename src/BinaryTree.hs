module BinaryTree(fromList, insert, find, delete, printTree) where

data (Ord a) => Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)

insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf x = (Node x Leaf Leaf)
insert (Node k left right) x
    | x == k    = Node x left right
    | x > k     = Node k left (insert right x)
    | otherwise = Node k (insert left x) right

fromList :: (Ord a) => [a] -> Tree a
fromList x = foldl insert Leaf x

find :: (Ord a) => Tree a -> a -> Bool
find Leaf m = False
find (Node k left right) m
    | k == m    = True
    | k < m     = find right m
    | otherwise = find left m

helper :: (Ord a) => Tree a -> (a, Tree a)
helper (Node m Leaf hr) = (m, hr)
helper (Node m hl hr) = do
    let (key, subTree) = helper hl
    (key, (Node m subTree hr))

deleteRoot :: (Ord a) => Tree a -> Tree a
deleteRoot (Node _ Leaf Leaf) = Leaf
deleteRoot (Node _ Leaf right) = right
deleteRoot (Node _ left Leaf) = left
deleteRoot (Node _ left (Node y Leaf right)) = (Node y left right)
deleteRoot (Node _ left right) = (Node new_key left new_sub) where
    (new_key, new_sub) = helper right

delete :: (Ord a) => Tree a -> a -> Tree a
delete Leaf k = Leaf
delete (Node x left right) k
    | x < k     = Node x left (delete right k)
    | x > k     = Node x (delete left k) right
    | otherwise = deleteRoot (Node x left right)

printTree :: (Ord a, Show a) => Tree a -> IO ()
printTree = mapM_ putStrLn . treeIndent
  where
    treeIndent Leaf           = ["-- /-"]
    treeIndent (Node k lb rb) =
      ["--" ++ show k] ++
      map ("  |" ++) ls ++
      ("  `" ++ r) : map ("   " ++) rs
      where
        (r:rs) = treeIndent rb
        ls     = treeIndent lb


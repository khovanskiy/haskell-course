module HW2.TreeMap(Tree(..), fromList) where
import HW2.Map

data Tree k v = Leaf | Node (k, v) (Tree k v) (Tree k v) deriving (Show)

fromList :: (Ord k) => [(k, v)] -> Tree k v
fromList [] = Leaf
fromList xs = foldl (\l (a, b) -> put l a b) Leaf xs

instance Map Tree where
    put Leaf k v = (Node (k, v) Leaf Leaf)
    put (Node (a, b) left right) k v = case compare k a of
        EQ  -> Node (k, v) left right
        LT  -> Node (a, b) (put left k v) right
        _   -> Node (a, b) left (put right k v)

    getOrDefault Leaf _ d = d
    getOrDefault (Node (a, b) left right) k d = case compare k a of
        EQ  -> b
        LT  -> getOrDefault left k d
        _   -> getOrDefault right k d

    get Leaf _ = Nothing
    get (Node (a, b) left right) k = case compare k a of
        EQ  -> Just b
        LT  -> get left k
        _   -> get right k
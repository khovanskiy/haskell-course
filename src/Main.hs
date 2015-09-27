module Main where

--import Data.Monoid
--import CachedSumList
--import BinaryTree
import ListMap

main = do
    ---let example = (Node 5 (Node 4 Leaf Leaf) (Node 19 (Node 6 Leaf (Node 7 Leaf Leaf)) (Node 12 Leaf Leaf)))
    --let arr = [5, 4, 19, 19, 19, 25, 23, 21, 22, 24, 15, 10, 17]
    --print $ fromList arr
    --let a = fromList [5, 4, 19, 19, 19, 25, 23] :: Tree Int
    --let t = fromList a :: Tree Int
    --let b = fromList [23, 21, 22, 24, 15, 10, 17] :: Tree Int
    --let c = fromList [12, 44, 54, 98];
    --let ab = toList (delete a 19)

    let arr = [("A", 5 :: Int), ("B", 7 :: Int), ("C", 1 :: Int), ("D", 4 :: Int), ("E", 10 :: Int)]
    let m = fromList arr
    print $ m
    print $ getOrDefault m "G" (-1)
    --let test = fromList arr
    --print $ map (\x -> (find test x)) arr
    --l-et test = insert Leaf 5
    ---let test2 = insert test 4
    --printTree $ (delete test 5)
    ---print $ helper test
    --print $ "Hello"--(randomIntList 10 10 10)




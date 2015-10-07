module Main where

--import Data.Monoid
--import CachedSumList
--import BinaryTree
--import ListMap
--import TreeMap
--import MaybeMap
--import Diagnostic
--import MassCenter(testMassCenter)
--import FlipNeighbour
--import CycledList(testCycledList)
import Factor

main :: IO()
main = do
    ---let example = (Node 5 (Node 4 Leaf Leaf) (Node 19 (Node 6 Leaf (Node 7 Leaf Leaf)) (Node 12 Leaf Leaf)))
    --let array = [5, 4, 19, 19, 19, 25, 23, 21, 22, 24, 15, 10, 17]
    --let set = fromList array :: Tree Int
    --print $ nextN set 19 6
    testFactor
    --print $ fromList arr
    --let a = fromList [1, 2, 3, 4];
    --let b = shift a
    --let c = shift b
    --let d = shift c
    --let e = shift d
    --print $ a
    --print $ b
    --print $ c
    --print $ d
    --print $ e
    --print $ toList e



    --let a = fromList [5, 4, 19, 19, 19, 25, 23] :: Tree Int
    --print $ next a 19
    --let t = fromList a :: Tree Int
    --let b = fromList [23, 21, 22, 24, 15, 10, 17] :: Tree Int
    --let c = fromList [12, 44, 54, 98];
    --let ab = toList (delete a 19)

    --let a = fromList [("A", 5 :: Int), ("B", 7 :: Int), ("C", 1 :: Int), ("D", 4 :: Int), ("E", 10 :: Int)]
    --let b = fromList [("G", 11 :: Int), ("D", 55 :: Int), ("C", 2 :: Int)]
    --print $ a
    --print $ b
    --print $ getOrDefaultWith [a, b] "F" (-1)
    --let r = Range (3, 4)

    --let error1 = D (0, 3) "Error 1" :: Diagnostic Error
    --let error2 = D (2, 9) "Error 2" :: Diagnostic Error
    --let warning1 = D (3, 4) "Warning 1" :: Diagnostic Warning
    --let warning2 = D (3, 4) "Warning 2" :: Diagnostic Warning
    --print $ error1 <> error2

    --let a = fromPair ("A", 5) :: Value String Int
    --print $ getOrDefault a "A" 7
    --print $ getOrDefaultWith [Value ("A", Just 5 :: Maybe Int), Value ("B", Just 7 :: Maybe Int)] "A" (-1)
    --let test = fromList arr
    --print $ map (\x -> (find test x)) arr
    --l-et test = insert Leaf 5
    ---let test2 = insert test 4
    --printTree $ (delete test 5)
    ---print $ helper test
    --print $ "Hello"--(randomIntList 10 10 10)




module Main where

--import Data.Monoid
--import CachedSumList
--import MassCenter(testMassCenter)
--import FlipNeighbour
--import CycledList(testCycledList)
--import Control.Monad
--import BinaryTree
--import HW3.Factor
--import HW3.State
import HW4.IntegerParser

main :: IO()
main = do
    ---let example = (Node 5 (Node 4 Leaf Leaf) (Node 19 (Node 6 Leaf (Node 7 Leaf Leaf)) (Node 12 Leaf Leaf)))
    --let array = [5, 4, 19, 19, 19, 25, 23, 21, 22, 24, 15, 10, 17]
    testIntegerParser
    --- print $ runParser parse "1 + (2 * 3 *( 4 - 5))^4"

    --let a = fromList [5, 4, 19, 19, 19, 25, 23] :: Tree Int
    --print $ next a 19
    --let t = fromList a :: Tree Int
    --let b = fromList [23, 21, 22, 24, 15, 10, 17] :: Tree Int
    --let c = fromList [12, 44, 54, 98];
    --let ab = toList (delete a 19)

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




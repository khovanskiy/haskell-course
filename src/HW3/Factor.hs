module HW3.Factor(factor, testFactor) where

import Asserts
import MergeSort

helper :: Int -> Int -> [Int]
helper n k
    | n == k            = [n]
    | n `mod` k == 0    = k : helper (n `div` k) k
    | otherwise         = helper n (k + 1)

factor :: [Int] -> [Int]
factor x = mergeSort $ x >>= \t -> helper t 2

testFactor :: IO()
testFactor = do
    Asserts.equals "F:  Example #1" [3, 3, 7, 11, 13, 17] $ factor [9, 17, 1001]
    Asserts.equals "F:  Example #2" [2, 2, 2, 2, 2, 2, 2, 3] $ factor [8, 16, 3]



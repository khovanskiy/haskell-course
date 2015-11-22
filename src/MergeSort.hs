module MergeSort(mergeSort, naiveQsort) where

import System.Random

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort x = do
     let (left, right) = split x where
         split :: [a] -> ([a], [a])
         split (x:y:zs)  = (x:xs, y:ys) where
             (xs, ys) = split zs
         split x         = (x, [])
     merge (mergeSort left) (mergeSort right) where
         merge :: (Ord a) => [a] -> [a] -> [a]
         merge l [] = l
         merge [] r = r
         merge (x:xs) (y:ys)
             | x <= y    = x : (merge xs (y:ys))
             | otherwise = y : (merge (x:xs) ys)

naiveQsort :: (Ord a) => [a] -> [a]
naiveQsort [] = []
naiveQsort (x:xs) = naiveQsort lesser ++ [x] ++ naiveQsort greater where
    lesser  = [i | i <- xs, i < x]
    greater = [i | i <- xs, i >= x]

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = fmap (take n . randomRs (from, to) . mkStdGen) randomIO

-- usage: fmap mergeSort $ randomIntList size fromInclusive endInclusive


module HW8.SpeedTest(speedTest) where

import Criterion.Main
import System.Random
import Control.Monad

import Data.List(sort)
import MergeSort(mergeSort, naiveQsort)
import HW8.QuickSort(quickSort)
import HW8.TreeSort(treeSort)
import qualified HW8.ListInsertionSort  as Lst
import qualified HW8.ArrayInsertionSort as Arr
import qualified HW8.VecInsertionSort   as Vec

sortedUp :: Int -> [Int]
sortedUp t = [0..10 ^ t]

sortedDown :: Int -> [Int]
sortedDown t = [10 ^ t, (10 ^ t - 1)..0]

randList :: Int -> [Int]
randList t = take (10 ^ t) $ randomRs (0, 10 ^ t) (mkStdGen t)

orderedList :: Int -> [Int]
orderedList t | t >= 2 = join $ replicate (10 ^ (t - 2)) [1..100]

benches :: [[Int] -> Benchmark]
benches = [
        bench "merge" . nf mergeSort,
        bench "sort" . nf sort,
        bench "lst" . nf Lst.sort,
        bench "arr" . nf Arr.sort,
        bench "vec" . nf Vec.sort,
        bench "tree" . nf treeSort,
        bench "naive" . nf naiveQsort,
        bench "qsort" . nf quickSort
    ]

benches2 :: [[Int] -> Benchmark]
benches2 = [
        bench "merge" . nf mergeSort,
        bench "sort" . nf sort,
--        bench "lst" . nf Lst.sort,
--        bench "arr" . nf Arr.sort,
--        bench "vec" . nf Vec.sort,
--        bench "tree" . nf treeSort,
--        bench "naive" . nf naiveQsort,
        bench "qsort" . nf quickSort
    ]

runBenches :: [[Int] -> Benchmark] -> [Int] -> [Benchmark]
runBenches bs arr = map (\b -> b arr) bs

speedTest :: IO ()
speedTest = defaultMain [
        bgroup "10^3" [
            bgroup "sorted up" $ runBenches benches (sortedUp 3),
            bgroup "sorted down" $ runBenches benches (sortedDown 3),
            bgroup "random" $ runBenches benches (randList 3),
            bgroup "ordered" $ runBenches benches (orderedList 3)
        ],
        bgroup "10^5" [
            bgroup "sorted up" $ runBenches benches2 (sortedUp 5),
            bgroup "sorted down" $ runBenches benches2 (sortedDown 5),
            bgroup "random" $ runBenches benches2 (randList 5),
            bgroup "ordered" $ runBenches benches2 (orderedList 5)
        ]
    ]



module B1.SpeedTest where

import Criterion.Main
import System.Random
import Control.Monad

import Set
import BinaryTree
import B1.AVLTree

f1 :: [Int] -> Bool
f1 arr = do
    let p = Set.fromList arr :: BinaryTree.Tree Int
    any (contains p) arr

f2 :: [Int] -> Bool
f2 arr = do
    let p = Set.fromList arr :: B1.AVLTree.Tree Int
    any (contains p) arr

benches :: [[Int] -> Benchmark]
benches = [
    bench "naive" . nf f1,
    bench "avl" . nf f2
    ]

runBenches :: [[Int] -> Benchmark] -> [Int] -> [Benchmark]
runBenches bs arr = map (\b -> b arr) bs

sortedUp :: Int -> [Int]
sortedUp t = [0..10 ^ t]

sortedDown :: Int -> [Int]
sortedDown t = [10 ^ t, (10 ^ t - 1)..0]

randList :: Int -> [Int]
randList t = take (10 ^ t) $ randomRs (0, 10 ^ t) (mkStdGen t)

orderedList :: Int -> [Int]
orderedList t | t >= 2 = join $ replicate (10 ^ (t - 2)) [1..100]

speedTest :: IO ()
speedTest = defaultMain [
        bgroup "10^3" [
            bgroup "sorted up" $ runBenches benches (sortedUp 3),
            bgroup "sorted down" $ runBenches benches (sortedDown 3),
            bgroup "random" $ runBenches benches (randList 3),
            bgroup "ordered" $ runBenches benches (orderedList 3)
        ],
        bgroup "10^5" [
            bgroup "sorted up" $ runBenches benches (sortedUp 5),
            bgroup "sorted down" $ runBenches benches (sortedDown 5),
            bgroup "random" $ runBenches benches (randList 5),
            bgroup "ordered" $ runBenches benches (orderedList 5)
        ]
    ]
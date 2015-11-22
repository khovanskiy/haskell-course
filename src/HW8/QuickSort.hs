{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module HW8.QuickSort(quicksort, quicksort_) where

import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Control.Monad
import System.Random
import Control.Monad.Loops (whileM_)

quicksort :: [Int] -> [Int]
quicksort list = runST $ do
    let listSize = length list
    arr <- newListArray (0, listSize - 1) list :: ST s (STUArray s Int Int)
    quicksort_ arr (0 :: Int) (listSize - 1) (mkStdGen 42)
    getElems arr

quicksort_ :: (Ord e, MArray a e (ST s)) => a Int e -> Int -> Int -> StdGen -> ST s ()
quicksort_ arr low high oldG = do
    i <- newSTRef low
    j <- newSTRef high
    ---let middle = (low + high) `div` 2
    let (middle, newG) = randomR (low, high) oldG
    x <- readArray arr middle
    whileM_ (do ii <- readSTRef i; jj <- readSTRef j; return $ ii <= jj) $ do
        whileM_ (do ii <- readSTRef i; ai <- readArray arr ii; return $ ai < x) $ do
            modifySTRef i (\p -> p + 1)
        whileM_ (do jj <- readSTRef j; aj <- readArray arr jj; return $ aj > x) $ do
            modifySTRef j (\p -> p - 1)
        ii <- readSTRef i
        jj <- readSTRef j
        when (ii <= jj) $ do
            ai <- readArray arr ii
            aj <- readArray arr jj
            writeArray arr ii aj
            writeArray arr jj ai
            writeSTRef i (ii + 1)
            writeSTRef j (jj - 1)
    ii <- readSTRef i
    jj <- readSTRef j
    when (low < jj) $ do
        quicksort_ arr low jj newG
    when (ii < high) $ do
        quicksort_ arr ii high newG
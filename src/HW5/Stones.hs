module HW5.Stones(manHeaps, zeroInMin) where

import Control.Monad.Writer
import Data.Functor

isCorrectHeaps :: (Int, Int) -> Bool
isCorrectHeaps (x, y) = x >= 0 && y >= 0

manHeaps :: Writer String (Int, Int) -> [Writer String (Int, Int)]
---manHeaps (a, b) = writer <$> [((a - 1, b), "a - 1"), ((a, b - 1), "b - 1"), ((a `div` 2, b * 2), "a / 2, b * 2"), ((a * 2, b `div` 2), "a * 2, b / 2")]
manHeaps w = do
    let ((a, b), oldlog) = runWriter w
    writer <$> (\p -> (p, oldlog ++ " -> " ++ show p)) <$> [(a - 1, b), (a, b - 1), (a `div` 2, b * 2), (a * 2, b `div` 2)]

---zeroIn3 :: (Int, Int) -> Bool
---zeroIn3 h = any (\(a, b) -> a == 0 && b == 0) $ [h] >>= manHeaps >>= (\manHeaps >>= manHeaps

filterW :: ((Int, Int) -> Bool) -> [Writer String (Int, Int)] -> [Writer String (Int, Int)]
filterW p ws = filter isCorrectWriter ws where
    isCorrectWriter = p . fst . runWriter

helper :: Int -> [Writer String (Int, Int)] -> Writer String Int
helper k h = do
    let result = filterW isCorrectHeaps $ h >>= manHeaps
    let filtered = filterW (\(a, b) -> a == 0 && b == 0) result
    if null filtered then helper (k + 1) result else do
        let (_, flog) = runWriter $ head filtered
        writer (k, flog)

zeroInMin :: (Int, Int) -> Writer String Int
zeroInMin p = helper 0 [return p]

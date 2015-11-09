{-# LANGUAGE NoImplicitPrelude #-}
module CW2.FilterAndReplicate(filter, replicate) where

import Asserts
--- Написать filter и replicate через монаду список ---

import Prelude(Int, Bool, even, odd, IO)
import Control.Monad
import Data.Monoid

filter :: (a -> Bool) -> [a] -> [a]
filter f xs = xs >>= (\x -> if (f x) then (return x) else mempty)

replicate :: Int -> a -> [a]
replicate n x = [1..n] >>= \_ -> [x]

--- Тесты ---
testFR :: IO ()
testFR = do
    Asserts.equals "#1" [2, 4, 6, 8, 10] (filter even [1..10])
    Asserts.equals "#2" [1,3,5,7,9] (filter odd [1..10])

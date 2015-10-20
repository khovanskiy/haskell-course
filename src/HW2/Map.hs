--{-# LANGUAGE MultiParamTypeClasses #-}
module HW2.Map(Map(..)) where

class Map m where
    put :: (Ord k) => m k v -> k -> v -> m k v
    get :: (Ord k) => m k v -> k -> Maybe v
    getOrDefault :: (Ord k) => m k v -> k -> v -> v
    getOrDefaultWith :: (Ord k, Eq v) => [m k v] -> k -> v -> v
    getOrDefaultWith [] _ d = d
    getOrDefaultWith (x:xs) k d = let res = (getOrDefault x k d) in
        if d == res then getOrDefaultWith xs k d else res


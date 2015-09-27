{-# LANGUAGE MultiParamTypeClasses #-}
module Map where

class Map m where
    put :: (Ord k) => m k v -> k -> v -> m k v
    getOrDefault :: (Ord k) => m k v -> k -> v -> v


{-# LANGUAGE FlexibleInstances #-}
module Set(Set(..)) where

import Data.Monoid

class Set s where
    emptySet :: s a
    toList :: s a -> [a]

    fromList :: (Ord a) => [a] -> s a
    --fromList [] = emptySet
    fromList = foldl insert emptySet

    find    :: (Ord a) => s a -> a -> Maybe a
    insert  :: (Ord a) => s a -> a -> s a
    delete  :: (Ord a) => s a -> a -> s a

    next    :: (Ord a) => s a -> a -> Maybe a
    next t m = case filter (\k -> k > m) (toList t) of
        []      -> Nothing
        (x:_)   -> Just x

    nextN   :: (Ord a) => s a -> a -> Int -> Maybe a
    nextN s m k
        | k < 0     = Nothing
        | k == 0    = find s m
        | k == 1    = next s m
        | otherwise = next s m >>= (\x -> nextN s x (k - 1))

instance (Ord a, Set s) => Monoid (s a) where
    mempty = emptySet
    mappend a b = fromList $ toList a ++ toList b


{-# LANGUAGE FlexibleInstances #-}
module Set where

import Data.Monoid

class Set s where
    emptySet :: s a
    toList :: s a -> [a]

    fromList :: (Ord a) => [a] -> s a
    fromList [] = emptySet
    fromList x = foldl insert emptySet x

    find     :: (Ord a) => s a -> a -> Maybe a
    insert   :: (Ord a) => s a -> a -> s a
    delete   :: (Ord a) => s a -> a -> s a

    next     :: (Ord a) => s a -> a -> Maybe a
    next t m = case filter (\k -> k > m) (toList t) of
        []      -> Nothing
        (x:_)   -> Just x

instance (Ord a, Set s) => Monoid (s a) where
    mempty = emptySet
    mappend a b = fromList $ toList a ++ toList b


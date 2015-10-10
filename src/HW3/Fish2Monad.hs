{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module HW3.Fish2Monad where

import HW3.FishAndJoin

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
--(>=>)       :: (a -> m b) -> (b -> m c) -> (a -> m c)

instance MonadFish m => Monad m where
    return      = returnFish
    ma >>= k    = (id >=> k) ma
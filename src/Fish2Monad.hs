{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module Fish2Monad where

import FishAndJoin

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
--(>=>)       :: (a -> m b) -> (b -> m c) -> (a -> m c)

const :: (Monad m) => m a -> b -> m a
const m _ = m

instance MonadFish m => Monad m where
    return      = returnFish
    ma >>= k    = (const ma) >=> k
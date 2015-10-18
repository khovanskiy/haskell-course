{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module HW3.Fish2Join where

import HW3.FishAndJoin

--join  x     = x >>= id
--join        :: m (m a) -> m a
--(>=>)       :: (a -> m b) -> (b -> m c) -> (a -> m c)
--x >>= id    = (id >=> id) x

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join = id >=> id

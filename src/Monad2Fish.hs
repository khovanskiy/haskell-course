{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module Monad2Fish where

import FishAndJoin

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

instance Monad m => MonadFish m where
    returnFish  = return
    f >=> g     = \x -> f x >>= g

id :: a -> a
id a = a

instance Monad m => MonadJoin m where
    returnJoin  = return
    join  x     = x >>= id
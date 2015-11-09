{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module CW2.Join2Fish where

--- MonadJoin => MonadFish

import Data.Functor

class MonadFish m where
    returnFish  :: a -> m a
    (>=>)       :: (a -> m b) -> (b -> m c) -> (a -> m c)

class MonadJoin m where
    returnJoin  :: a -> m a
    join        :: m (m a) -> m a

--- f >=> g     = \x -> (f x >>= g)
--- ma >>= k = join (fmap k ma)
--- \x -> (f x >>= g) = \x -> join (fmap g (f x))

instance (MonadJoin m, Functor m) => MonadFish m where
    returnFish = returnJoin
    f >=> g = \x -> join (fmap g (f x))
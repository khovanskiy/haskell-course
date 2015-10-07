{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module Join2Monad where

import FishAndJoin

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

fmap :: (Monad m) => (a -> b) -> m a -> m b
fmap f x = x >>= (\t -> return (f t))

instance MonadJoin m => Monad m where
    return = returnJoin
    ma >>= k = join (fmap k ma)



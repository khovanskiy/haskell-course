{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module HW3.Monad(Monad(..), fmap) where

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

fmap :: (Monad m) => (a -> b) -> m a -> m b
fmap f x = x >>= (\t -> return (f t))
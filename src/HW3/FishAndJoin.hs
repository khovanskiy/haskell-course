{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module HW3.FishAndJoin(MonadFish(..), MonadJoin(..), id) where

class MonadFish m where
    returnFish  :: a -> m a
    (>=>)       :: (a -> m b) -> (b -> m c) -> (a -> m c)

class MonadJoin m where
    returnJoin  :: a -> m a
    join        :: m (m a) -> m a

id :: a -> a
id a = a
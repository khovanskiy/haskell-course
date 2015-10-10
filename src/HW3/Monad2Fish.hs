{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module HW3.Monad2Fish where

import HW3.FishAndJoin
import HW3.Monad

instance Monad m => MonadFish m where
    returnFish  = return
    f >=> g     = \x -> f x >>= g

instance Monad m => MonadJoin m where
    returnJoin  = return
    join  x     = x >>= id
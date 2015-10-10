{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module HW3.Join2Monad where

import HW3.FishAndJoin
import HW3.Monad

instance MonadJoin m => Monad m where
    return = returnJoin
    ma >>= k = join (fmap k ma)



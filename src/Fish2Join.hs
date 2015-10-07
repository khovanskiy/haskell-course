{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module Fish2Join where

import FishAndJoin

instance MonadFish m => MonadJoin m where
    returnFish = returnJoin

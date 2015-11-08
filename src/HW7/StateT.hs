{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module HW7.StateT where

import Control.Monad.State.Class(MonadState(..))
import Control.Monad.Trans.Class(MonadTrans(..))

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Monad m) => Monad (StateT s m) where
    return a = state $ \ s -> (a, s)
    m >>= k  = StateT $ \ s -> do
        (a, s') <- runStateT m s
        runStateT (k a) s'

instance MonadTrans (StateT s) where
    lift m = StateT $ \ s -> do
        a <- m
        return (a, s)

instance Monad m => MonadState s (StateT s m) where
    get = StateT $ return . \s -> (s, s)
    put s = StateT $ return . \_ -> ((), s)
    state f = do
        s <- get
        let (a, s') = f s
        put s'
        return a
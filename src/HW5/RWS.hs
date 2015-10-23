module HW5.RWS where

import Control.Applicative
import Control.Monad
import Data.Monoid

newtype RWS r w s a = RWS { runRWS :: r -> s -> (a, s, w) }

instance Functor (RWS r w s) where
    fmap f rws  = RWS $ \r s -> do
        let (x, y, z) = runRWS rws r s
        (f x, y, z)

instance (Monoid w) => Applicative (RWS r w s) where
    pure  = return
    (<*>) = ap

instance (Monoid w) => Monad (RWS r w s) where
    return a    = RWS $ \_ s -> (a, s, mempty)
    m >>= k     = RWS $ \r s -> do
        let (a, s',  w)  = runRWS m r s
        let (b, s'', w') = runRWS (k a) r s'
        (b, s'', w <> w')

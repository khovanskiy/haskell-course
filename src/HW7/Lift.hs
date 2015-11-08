module HW7.Lift where

liftM   :: (Monad m) => (a -> r) -> m a -> m r
liftM f m = do
    x <- m
    return (f x)


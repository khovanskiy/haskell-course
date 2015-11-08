module HW7.EitherT where

import Control.Monad(liftM)
import Control.Monad.Trans.Class(MonadTrans(..))

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Monad m => Monad (EitherT l m) where
  return a = EitherT $ return (Right a)
  m >>= k  = EitherT $ do
    a <- runEitherT m
    case a of
      Left  l -> return (Left l)
      Right r -> runEitherT (k r)

instance MonadTrans (EitherT l) where
  lift = EitherT . liftM Right
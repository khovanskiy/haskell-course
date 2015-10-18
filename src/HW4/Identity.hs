module HW4.Identity(Identity(..)) where

import Data.Foldable
import Control.Applicative
import Data.Traversable

newtype Identity a = Identity { runIdentity :: a} deriving (Eq, Ord, Read, Show)

instance Functor Identity where
    fmap f m = Identity $ f (runIdentity m)

instance Foldable Identity where
    foldMap f (Identity x) = f x

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity x = Identity (f x)

instance Traversable Identity where
    traverse f (Identity x) = fmap Identity (f x)
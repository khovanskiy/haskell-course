{-# LANGUAGE NoImplicitPrelude #-}
module HW4.Comma where

--import Data.Foldable(Foldable(foldMap))
--import Control.Applicative(Applicative(..))
--import Data.Traversable(Traversable(traverse))
--import Data.Functor(Functor(fmap))
import Data.Monoid

class  Functor f  where
    fmap        :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

class Foldable t where
    foldMap :: Monoid m => (a -> m) -> t a -> m
    foldMap f = foldr (mappend . f) mempty

    foldr :: (a -> b -> b) -> b -> t a -> b

id :: a -> a
id x = x

f :: ([a] -> [b] -> ([a],[b])) -> [a] -> [b] -> ([a],[b])
f g = g

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f = sequenceA . fmap f

    sequenceA :: Applicative f => t (f a) -> f (t a)
    sequenceA = traverse id

instance Functor ((,) a) where
    fmap f (x, y) = (x, f y)

instance Foldable ((,) a) where
    foldr f z (_, y) = f y z

instance Traversable ((,) a) where
    traverse f (x, y) = fmap ((,) x) (f y)

instance Monoid a => Applicative ((,) a) where
    pure x = (mempty, x)
    (u, f) <*> (v, x) = (u <> v, f x)


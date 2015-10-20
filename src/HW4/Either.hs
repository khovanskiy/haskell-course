{-# LANGUAGE NoImplicitPrelude #-}
module HW4.Either(Either(..)) where

import GHC.Base(Ord, Eq)
import GHC.Read
import GHC.Show
import Data.Foldable
import Control.Applicative
import Data.Traversable
import Data.Functor
import Data.Monoid

data Either a b  =  Left a | Right b deriving (Eq, Ord, Read, Show)

instance Functor (Either a) where
    fmap _ (Left x)  = Left x
    fmap f (Right y) = Right (f y)

instance Foldable (Either a) where
    foldMap _ (Left _)  = mempty
    foldMap f (Right x) = f x

instance Applicative (Either a) where
    pure          = Right
    Left  e <*> _ = Left e
    Right f <*> r = fmap f r

instance Traversable (Either a) where
    traverse _ (Left x)  = pure (Left x)
    traverse f (Right x) = fmap Right (f x)
module HW4.Const(Constant(..)) where

import Data.Foldable
import Control.Applicative
import Data.Traversable
import Data.Monoid

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord)

instance Functor (Constant a) where
    fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
    foldMap _ _ = mempty

instance Traversable (Constant a) where
    traverse _ (Constant x) = pure (Constant x)

instance (Monoid a) => Applicative (Constant a) where
    pure _ = Constant mempty
    Constant x <*> Constant y = Constant (x <> y)
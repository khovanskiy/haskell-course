module HW4.Tree(Tree(..)) where

import Data.Foldable
import Control.Applicative
import Data.Traversable
import BinaryTree
import Data.Monoid

instance Functor Tree where
    fmap _ Leaf = Leaf
    fmap f (Node k l r) = Node (f k) (fmap f l) (fmap f r)

instance Foldable Tree where
    foldMap _ Leaf = mempty
    foldMap f (Node k l r) = foldMap f l <> f k <> foldMap f r

instance Applicative Tree where
    pure a = Node a Leaf Leaf
    Leaf <*> Leaf = Leaf
    Node fk fl fr <*> Node vk vl vr = Node (fk vk) (fl <*> vl) (fr <*> vr)

instance Traversable Tree where
    traverse _ (Leaf) = pure Leaf
    traverse f (Node k l r) = Node <$> f k <*> traverse f l <*> traverse f r


module B2.Laws where

import Data.Functor
import Control.Applicative

--- # Functor laws
--- 1)  fmap id = id
--- 2)  fmap (f . g) = fmap f . fmap g
---     fmap (f . g) F = fmap f (fmap g F)

--- # Applicative laws
--- ## identity
--- 1) pure id <*> v = v
--- ## composition
--- 2) pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--- ## homomorphism
--- 3) pure f <*> pure x = pure (f x)
--- ## interchange
--- 4) u <*> pure y = pure ($ y) <*> u

--- fmap id m == fmap id (Just x) == (Just x) == id (Just x) = id m
--- fmap (f . g) m == fmap (f . g) (Just x) == Just (f (g x)) == fmap f (fmap g (Just x)) == fmap f (fmap g m)
--- pure id <*> v == Just id <*> Just x == Just (id x) == Just x == v
--- pure (.) <*> u <*> v <*> w == Just (.) <*> Just u1 <*> Just v1 <*> Just w1 == Just ((u1 . v1) w1) == Just (u1 (v1 w1)) == u <*> (v <*> w)
--- pure f <*> pure x = Just f <*> Just x == Just (f x) == pure (f x)
--- u <*> pure y = Just u1 <*> Just y == Just (u1 y) == Just ($ y) <*> Just u1 == pure ($ y) <*> u

--- fmap id m == fmap id Nothing == Nothing == id Nothing = id m
--- fmap (f . g) m == fmap (f . g) Nothing == Nothing == fmap f (fmap g Nothing) == fmap f (fmap g m)
--- pure id <*> v == Just id <*> Nothing == Nothing == v
--- pure (.) <*> u <*> v <*> w == Just (.) <*> Nothing <*> Nothing <*> Nothing == Nothing == Nothing <*> (Nothing <*> Nothing) == u <*> (v <*> w)
--- u <*> pure y = Nothing <*> Just y == Nothing == Just ($ y) <*> Nothing == pure ($ y) <*> u

--- fmap id m == fmap id [x, y, z] == [x, y, z] == id [x, y, z] = id m
--- fmap (f . g) m == fmap (f . g) [x, y, z] == [f (g x), f (g y), f (g z)] == fmap f (fmap g [x, y, z]) == fmap f (fmap g m)
--- pure id <*> v == pure id <*> [x, y, z] == [id] <*> [x, y, z] == [x, y, z] == v
--- pure (.) <*> u <*> v <*> w == (.) <*> [u1, u2] <*> [v1, v2] <*> [w1, w2, w3] == [u1 . v1, u1 . v2, u2 . v1, u2 . v2] <*> [w1, w2, w3] ==
--- == [(u1 . v1) w1, (u1 . v2) w1, (u2 . v1) w1, (u2 . v2) w1, (u1 . v1) w2, (u1 . v2) w2, (u2 . v1) w2, (u2 . v2) w2, (u1 . v1) w3, (u1 . v2) w3, (u2 . v1) w3, (u2 . v2) w3]
--- == [u1, u2, u3] <*> ([v1, v2, v3] <*> [w1, w2, w3]) == u <*> (v <*> w)
--- pure f <*> pure x == [f] <*> [x] == [f x] == pure (f x)
--- u <*> pure y == [u1, u2, u3] <*> [w1, w2] == [u1 w1, u1 w1, u2 w1, u2 w2, u3 w1, u3 w2] == [($ [w1, w2])] <*> [u1, u2, u3] == pure ($ y) <*> u

--- fmap id m == fmap id (Const x) == Const x == id (Const x) = id m
--- fmap (f . g) m == fmap (f . g) (Const x) == Const x == fmap f (fmap g (Const x)) == fmap f (fmap g m)
--- pure id <*> v = Const mempty <*> Const x == Const (mempty <> x) == Const x == v
--- pure (.) <*> u <*> v <*> w = Const mempty <*> Const u1 <*> Const v1 <*> Const w1 == Const (u1 <> v1 <> w1) == pureu <*> (v <*> w)
--- pure f <*> pure x == Const mempty <*> Const mempty == Const mempty == pure (f x)
--- u <*> pure y = Const x <*> Const mempty == Const x == Const mempty <*> Const x == pure ($ y) <*> u


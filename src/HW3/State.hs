module HW3.State(State(..), testState) where

import Control.Monad
import Asserts

newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
    return a = State $ \s -> (a,  s)
    oldState >>= f = State $ \ st ->
       let (x, newState) = runState oldState st -- Running the first processor on st.
       in runState (f x) newState       -- Running the second processor on st'.

testState :: IO()
testState = do
    let f = \x -> if x `mod` 2 == 0 then ("Even", x + 1) else ("Odd", x + 1)
    let init = State f
    Asserts.equals "S:  Example #1" ("Even", 1) $ runState init 0
    Asserts.equals "S:  Example #2" ("Odd", 2) $ runState init 1
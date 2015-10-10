module HW3.State(State(..), testState) where

import Control.Monad
import Asserts

newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
    return a = State $ \s -> (a,  s)
    pr >>= k = State $ \ st ->
       let (x, st') = runState pr st -- Running the first processor on st.
       in runState (k x) st'       -- Running the second processor on st'.

testState :: IO()
testState = do
    let f = \x -> if x `mod` 2 == 0 then ("Even", x + 1) else ("Odd", x + 1)
    let init = State f
    print $ runState (init >>= (\x -> x + 1)) 0
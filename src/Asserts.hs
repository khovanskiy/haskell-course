module Asserts(equals) where

equals :: (Eq a) => String -> a -> a -> IO()
equals name expected actual
    | expected == actual    = putStrLn $ "Test \"" ++ name ++ "\" is passed"
    | otherwise             = putStrLn $ "Test \"" ++ name ++ "\" is failed: \""



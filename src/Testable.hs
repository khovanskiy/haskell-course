module Testable where

class Testable t where
    test :: t -> IO()

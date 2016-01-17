{-# LANGUAGE TemplateHaskell #-}
module HW9.FibCT(fibCT) where

import Language.Haskell.TH
import Data.Functor

fibCT :: Q Exp
fibCT = do
    let cache = (LitE . IntegerL) <$> (take 128 fibs) where
        fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
    return $ ListE cache
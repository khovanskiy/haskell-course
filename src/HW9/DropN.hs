{-# LANGUAGE TemplateHaskell #-}
module HW9.DropN(dropN) where

import Language.Haskell.TH
import Data.Functor

dropN :: Int -> Int -> Q Exp
dropN n m = do
    let xs = (\num -> mkName $ "v" ++ show num) <$> [1..(n - m)]
    let a = VarP <$> xs
    let b = VarE <$> xs
    return $ LamE [TupP $ (replicate m WildP) ++ a] (TupE $ b)


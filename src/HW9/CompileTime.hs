{-# LANGUAGE TemplateHaskell #-}
module HW9.CompileTime(compileTime') where

import Data.Time
import Language.Haskell.TH

compileTime' :: Q Exp
compileTime' = do
  curTime <- runIO getCurrentTime
  return $ LitE (StringL (show curTime))


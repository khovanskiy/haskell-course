{-# LANGUAGE TemplateHaskell #-}
module HW9.Using(compileTime) where

import HW9.CompileTime as CT

compileTime :: String
compileTime = $(CT.compileTime')


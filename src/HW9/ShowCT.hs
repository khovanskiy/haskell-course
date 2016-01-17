{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module HW9.ShowCT(fullShow) where

import Language.Haskell.TH
import Data.List

fullShow :: Name -> Q [Dec]
fullShow name = do
    TyConI (DataD _ _ _ [RecC constructor fields] _) <- reify name

    let names = map (\(name,_,_) -> name) fields

    let showField :: Name -> Q Exp
        showField name = [|\x -> s ++ " = " ++ show ($(global name) x)|] where
            s = nameBase name

    let showFields :: Q Exp
        showFields = listE $ map showField names

    let clz :: String
        clz = nameBase constructor

    [d|instance Show $(conT name) where
        show x = clz ++ "[" ++ (intercalate ", " (map ($ x) $(showFields))) ++ "]"|]
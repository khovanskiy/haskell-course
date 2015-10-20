module HW2.ListMap(List(..), fromList) where
import HW2.Map

data List k v = List [(k, v)] deriving (Show)

fromList :: (Ord k) => [(k, v)] -> List k v
fromList [] = List []
fromList xs = foldl (\l (a, b) -> put l a b) (List []) xs

instance Map List where
    put (List []) k v = List [(k, v)]
    put (List ((a, b):xs)) k v = case compare k a of
        EQ  -> List ((k, v):xs)
        LT  -> List ((k, v):(a, b):xs)
        GT  -> List ((a, b):ts) where
            (List ts) = put (List xs) k v

    getOrDefault (List []) _ d = d
    getOrDefault (List ((a, b):xs)) k d = case compare k a of
        EQ  -> b
        GT  -> getOrDefault (List xs) k d
        _   -> d

    get (List []) _ = Nothing
    get (List ((a, b):xs)) k = case compare k a of
        EQ  -> Just b
        GT  -> get (List xs) k
        _   -> Nothing
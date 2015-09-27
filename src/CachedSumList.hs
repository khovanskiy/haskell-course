module CachedSumList(fromList, addFirst, removeFirst, removeAt) where

data (Num a) => CachedSumList a = CachedSumList {inner_list :: [a], inner_sum :: a} deriving (Show)

fromList :: (Num a) => [a] -> CachedSumList a
fromList arg = CachedSumList arg (sum arg)

addFirst :: (Num a) => (CachedSumList a) -> a -> (CachedSumList a)
addFirst (CachedSumList a b) value = (CachedSumList (value : a) (value + b))

removeFirst :: (Num a) => (CachedSumList a) -> (CachedSumList a)
removeFirst (CachedSumList (x : xs) b) = CachedSumList xs (b - x)

removeAt :: (Num a, Eq p, Num p) => (CachedSumList a) -> p -> (CachedSumList a)
removeAt (CachedSumList (x : xs) b) i
    | i == 0    = CachedSumList xs (b - x)
    | otherwise = do
        let other = removeAt (CachedSumList xs b) (i - 1)
        (CachedSumList (x : (inner_list other)) (inner_sum other))

instance (Eq a) => Eq (CachedSumList a) where
    (CachedSumList _ a) == (CachedSumList _ b) = a == b

instance (Ord a) => Ord (CachedSumList a) where
    (CachedSumList _ a) <= (CachedSumList _ b) = a <= b


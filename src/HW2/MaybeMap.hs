module MaybeMap(Value, put, getOrDefault, fromPair, getOrDefaultWith) where
import HW2.Map

newtype Value k v = Value (k, Maybe v) deriving (Show)

fromPair :: (Ord k) => (k, v) -> Value k v
fromPair (k, v) = Value (k, Just v)

instance Map Value where
    put (Value _) k v = Value (k, Just v)

    getOrDefault (Value (_, Nothing)) _ d = d
    getOrDefault (Value (a, Just b)) k d = if a == k then b else d


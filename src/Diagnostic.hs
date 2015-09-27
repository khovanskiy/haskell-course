module Diagnostic(Diagnostic(..), Range, Warning, Error) where
import Data.Monoid

type Range = (Int, Int)

data Diagnostic t = D Range String deriving (Show)

data Warning = Warning
data Error = Error

overlaps :: Diagnostic t -> Diagnostic t -> Bool
overlaps (D (a, b) m1) (D (c, d) m2) = let l = max a c
                                           r = min b d
                                        in l <= r

instance Monoid (Diagnostic t) where
    mempty  = (D (0, 0) "")
    mappend (D (a, b) m1) (D (c, d) m2) = D (max a c, min b d) (m1 ++ m2)

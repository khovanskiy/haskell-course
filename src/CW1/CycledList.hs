module CycledList(List(..), helper, shift, grab, fromList, toList, (!!!), testCycledList) where

-- #3 Реализовать циклический список (shift циклически сдвигает вправо, (!!!) получает значение по индексу)
-- #4 Реализовать тайпкласс Listable с поддержкой toList и fromList

class Listable l where
    fromList :: [a] -> l a
    toList :: l a -> [a]

data List a = List [a] Int Int deriving (Show, Eq)

helper :: [a] -> (a, [a])
helper (x:[]) = (x, [])
helper (x:xs) = do
    let (a, b) = helper xs
    (a, x:b)

shift :: (List a) -> List a
shift (List [] i l) = (List [] i l)
shift (List (x:xs) i l) = do
    let (a, b) = helper xs
    List (a:x:b) ((i + 1) `mod` l) l

infixl 9  !!!
(!!!) :: (List a) -> Int -> a
(List xs i l) !!! p = xs !! ((i + p ) `mod` l)


grab :: [a] -> Int -> ([a], [a])
grab (x:xs) i
    | i == 0    = ([], (x:xs))
    | i == 1    = ([x], xs)
    | otherwise = do
        let (a, b) = grab xs (i - 1)
        (x:a, b)

instance Listable List where
    fromList [] = List [] 0 0
    fromList xs = List xs 0 (length xs)

    toList (List [] _ _) = []
    toList (List xs i _) = do
        let (a, b) = grab xs i
        b ++ a

testCycledList :: IO()
testCycledList = do
    putStrLn $ if (List [1, 2, 3, 4] 0 4) == fromList [1, 2, 3, 4] then "Passed" else "Failed"
    putStrLn $ if [3, 4, 1, 2] == toList (List [1, 2, 3, 4] 2 4) then "Passed" else "Failed"
    print $ shift (List [3, 4, 1, 2] 2 4)
    putStrLn $ if 3 == (List [1, 2, 3, 4] 2 4)!!!0 then "Passed" else "Failed"
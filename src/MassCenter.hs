module MassCenter(compute, testMassCenter) where

-- #1 Задан список пар -- точки в 2D, необходимо найти их центр масс

compute :: (Fractional a) => [(a, a)] -> (a, a)
compute xs = foldl (\(a, b) (c, d) -> (a + c / l, b + d / l)) (0, 0) xs where
        l = fromIntegral (length xs)

testMassCenter :: IO()
testMassCenter = do
    putStrLn $ if (0, 0) == compute [] then "Passed" else "Failed"
    putStrLn $ if (2, 2) == compute [(1, 5), (2, 4), (3, -3)] then "Passed" else "Failed"
    putStrLn $ if (0.5, 0.5) == compute [(0, 0), (1, 1)] then "Passed" else "Failed"

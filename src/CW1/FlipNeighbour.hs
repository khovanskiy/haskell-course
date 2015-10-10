module FlipNeighbour(flipNeighbour, testFlipNeighbour) where

-- #2 Поменять местами соседние элементы в списке

flipNeighbour :: [a] -> [a]
flipNeighbour [] = []
flipNeighbour [x] = [x]
flipNeighbour (a:b:xs) = (b:a:(flipNeighbour xs))

testFlipNeighbour :: IO()
testFlipNeighbour = do
    putStrLn $ if [2, 1, 4, 3] == flipNeighbour [1, 2, 3, 4] then "Passed" else "Failed"
    putStrLn $ if [4, 3, 5, 6, 1] == flipNeighbour [3, 4, 6, 5, 1] then "Passed" else "Failed"
    putStrLn $ if [1] == flipNeighbour [1] then "Passed" else "Failed"
    putStrLn $ if 0 == length (flipNeighbour []) then "Passed" else "Failed"
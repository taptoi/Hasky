module Euler11 ( euler11 ) where
    import Data.List(transpose)
    import Hasky(listsOf, ints)
    
    shiftRows :: [[Integer]] -> [[Integer]]
    shiftRows [] = []
    shiftRows [r] = [r]
    shiftRows (row:rows) = row : shiftRows' rows [0] 
                        where
                            shiftRows' [] zs = [] 
                            shiftRows' [r] zs = [zs ++ r]
                            shiftRows' (r:rs) zs = (zs ++ r) : shiftRows' rs (0 : zs)

    rotateCw = map reverse . transpose
    diagonals = map (filter (\x -> x /= 0)) . transpose . shiftRows
    allDiagonals m = (diagonals m) ++ (diagonals $ transpose m)
    allDiagsOf n = map (listsOf n) . allDiagonals
    adjacentXsOf n = map (listsOf n)
    adjacentYsOf n = (adjacentXsOf n) . transpose
    allAdjacentXsAndYsOf n m = (adjacentXsOf n m) ++ (adjacentYsOf n m)
    allAdjacentsOf n m = (allAdjacentXsAndYsOf n m) ++ (allDiagsOf n m)
    solve = maximum . map product . concat . allAdjacentsOf 4

    euler11 :: String -> Integer
    euler11 =  solve . map ints . lines
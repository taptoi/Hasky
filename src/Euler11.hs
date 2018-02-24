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
    diagonals m = map (filter (\x -> x /= 0)) $ transpose $ shiftRows m
    allDiagonals m = (diagonals m) ++ (diagonals $ transpose m)
    allSeqsOf n m = filter (not . null) ( (map (listsOf n) m) ++ map (listsOf n) (allDiagonals m) )
    solve m = maximum $ map (foldl1 (*)) (concat (allSeqsOf 4 m))

    euler11 :: String -> Integer
    euler11 s =  solve ( map ints (lines s) )
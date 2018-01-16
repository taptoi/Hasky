module Euler67 ( euler67 ) where

    pairs :: [Integer] -> [(Integer, Integer)]
    pairs [] = []
    pairs [s] = [(0,0)]
    pairs (frst : scnd : tl) = (frst, scnd) : pairs (scnd : tl)

    reducePairs :: [(Integer, Integer)] -> [Integer]
    reducePairs = map (\(x, y) -> max x y)

    euler67 :: [[Integer]] -> Integer
    euler67 rows = reduce rows
            where 
                reduce [] = 0
                reduce (x : xs) = 
                    case xs of
                        [] -> head x
                        _  -> reduce (parents' : (tail xs)) 
                                where
                                    children = x
                                    parents = head xs
                                    children' = reducePairs $ (pairs children)
                                    parents' = map (\(x, y) -> x + y) (zip parents children')

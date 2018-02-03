module Euler6 ( euler6 ) where

    sqr n = n * n
    sumSquares n = foldl (\acc n -> sqr n + acc) 0 [1 .. n]
    squareSums n = sqr $ foldl (\acc n -> n + acc) 0 [1 .. n]
    solve n = (squareSums n) - (sumSquares n)

    euler6 :: Integer
    euler6 = solve 100
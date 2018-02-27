module Euler12 ( euler12 ) where
    import Hasky (primeFactors)
    triangles = scanl1 (+) [ 1, 2 .. ]

    euler12 :: Integer
    euler12 = 1

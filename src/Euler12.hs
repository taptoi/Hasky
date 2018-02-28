module Euler12 ( euler12 ) where
    import Hasky (numFactors)
    triangles = scanl1 (+) [ 1, 2 .. ]
    triangleFactors = map (\t -> (t, numFactors t)) triangles

    euler12 :: Integer
    euler12 = fst . head . filter (\n -> snd n > 500) $ triangleFactors

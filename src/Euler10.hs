module Euler10 ( euler10 ) where
    import Hasky (primesTo)
    euler10 :: Integer
    euler10 = foldl (+) 0 (primesTo 2000000)

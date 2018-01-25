module Euler2 ( euler2 ) where
    
    fibList a b = [a] ++ fibList b (a + b)
    fibs = fibList 1 2
    evens xs = filter even xs
    euler2 :: Integer
    euler2 = sum $ evens $ takeWhile ( < 4000000 ) fibs
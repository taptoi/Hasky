module Euler3 ( euler3 ) where
    
    primes = 2 : filter (isPrime) [3, 5 ..]
    isPrime n = isPrime' n primes
                where 
                isPrime' n (p:ps)
                    | p * p > n      = True
                    | n `rem` p == 0 = False
                    | otherwise = isPrime' n ps


    maxPrimeFactor n = 
        maxPrimeFactor' n primes 2
        where
        maxPrimeFactor' c (p:ps) acc
            | p * p > n      = acc
            | c `rem` p /= 0 = maxPrimeFactor' c ps acc -- not a factor
            | c `rem` p == 0 = maxPrimeFactor' (c `quot` p) ps p -- prime factor found

    euler3 :: Integer
    euler3 = maxPrimeFactor 600851475143

module Hasky (primes, primesTo, maxPrimeFactor, isPrime, listsOf, digits, ints) where
    import Data.Char(digitToInt, isHexDigit)
    primes = 2 : filter (isPrime) [3, 5 ..]
    isPrime n = isPrime' n primes
                where 
                isPrime' n (p:ps)
                    | p * p > n      = True
                    | n `rem` p == 0 = False
                    | otherwise = isPrime' n ps

    primesTo n = takeWhile (<= n) primes

    maxPrimeFactor n = 
        maxPrimeFactor' n primes 2
        where
        maxPrimeFactor' c (p:ps) acc
            | p * p > n      = acc
            | c `rem` p /= 0 = maxPrimeFactor' c ps acc -- not a factor
            | c `rem` p == 0 = maxPrimeFactor' (c `quot` p) ps p -- prime factor found

    listsOf n xs = filter (\a -> length a == n) $ listsOf' xs 
            where 
                listsOf' [] = []
                listsOf' [d] = []
                listsOf' (d:ds) = take n ([d] ++ ds) : listsOf' ds

    digits s = map (toInteger . digitToInt) $ filter isHexDigit s
    ints s = map (\x -> read x :: Integer) (words s)

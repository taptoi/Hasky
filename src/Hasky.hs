module Hasky (primes, primesTo, maxPrimeFactor, primeFactors, isPrime, listsOf, digits, ints) where
    import Data.Char(digitToInt, isHexDigit)
    import Data.List(tails)
    primes = 2 : filter (isPrime) [3, 5 ..]
    isPrime n = isPrime' n primes
                where 
                isPrime' n (p:ps)
                    | p * p > n      = True
                    | n `rem` p == 0 = False
                    | otherwise = isPrime' n ps

    primesTo n = takeWhile (<= n) primes

    primeFactors n = 
        primeFactors' n primes []
        where
        primeFactors' c (p:ps) acc
            | p > c          = acc
            | c `rem` p /= 0 = primeFactors' c ps acc -- not a factor
            | c `rem` p == 0 = primeFactors' (c `quot` p) ps (p:acc) -- prime factor found

    maxPrimeFactor = maximum . primeFactors

    -- factors n = concat . map (\pf -> [pf * 1, pf * 2 .. (n-1)]) . primeFactors $ n

    pairs :: [a] -> [[a]]
    pairs xs = [ [x,y] | (x:rest) <- tails xs , y <- rest ]

    powers n = [n, n*n ..]

    listsOf n xs = filter (\a -> length a == n) $ listsOf' xs 
            where 
                listsOf' [] = []
                listsOf' [d] = []
                listsOf' (d:ds) = take n ([d] ++ ds) : listsOf' ds

    digits = map (toInteger . digitToInt) . filter isHexDigit
    ints = map (\x -> read x :: Integer) . words

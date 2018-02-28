module Hasky (primes, primesTo, maxPrimeFactor, primeFactors, isPrime, listsOf, digits, ints, numFactors) where
    import Data.Char(digitToInt, isHexDigit)
    import Data.List(tails)
    import Data.Maybe(fromJust)
    import Data.List(nub)
    
    primes = 2 : filter (isPrime) [3, 5 ..]
    isPrime n = isPrime' n primes
                where 
                isPrime' n (p:ps)
                    | p * p > n      = True
                    | n `rem` p == 0 = False
                    | otherwise = isPrime' n ps

    primesTo n = takeWhile (<= n) primes

    primeFactors n = 
        nub . reverse $ (primeFactors' n primes [])
        where
        primeFactors' c (p:ps) acc
            | p * p > n && null acc = [p]
            | p > c                 = acc
            | c `rem` p /= 0        = primeFactors' c ps acc -- not a factor
            | c `rem` p == 0        = primeFactors' (c `quot` p) (p:ps) (p:acc) -- prime factor found

    maxPrimeFactor = maximum . primeFactors

    -- if number prod has a prime factor of base, e.g. base^a = prod
    -- return the maximum exponent a of base that prod is divisible with
    primeFactorExponent prod base = primeFactorExponent' prod 0 
                                    where 
                                        primeFactorExponent' prod' acc 
                                            | prod' <= 0                         = Nothing
                                            | acc == 0 && prod' `rem` base == 0  = primeFactorExponent' (prod' `quot` base) 1
                                            | prod' `rem` base == 0              = primeFactorExponent' (prod' `quot` base) (acc + 1)
                                            | prod' `rem` base /= 0 || prod' == 1 = Just acc
                                            | otherwise                          = Nothing


    -- prime factors and their powers that divide n evenly
    -- for n = p_1^a · p_2^b ··· p_k^r, returns
    -- [(p_1, a), (p_2, b), .. (p_k, r)]
    primeFactorPowers :: Integer -> [(Integer, Integer)]
    primeFactorPowers n =  map (\pf -> (pf, fromJust ( primeFactorExponent n pf ))) . primeFactors $ n
  
    -- using the Product Rule of counting (http://en.wikipedia.org/wiki/Rule_of_product), we know that there will be
    -- m = (a + 1)·(b + 1)···(r + 1)
    numFactors = product . map (\pfp -> snd pfp + 1) . primeFactorPowers

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

module Euler7 ( euler7 ) where
    import Hasky (primes)
    solve = last . take 10001 $ primes

    euler7 :: Integer
    euler7 = solve
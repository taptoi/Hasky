module Euler4 ( euler4 ) where

    nums numDigits = [1 .. (10 ^ numDigits - 1)]
    products ns = concat $ map (\i -> map (\j -> i * j) ns) ns
    isPalindromeNumber i = show i == reverse (show i)
    allPalindromes ns = filter isPalindromeNumber (products ns)
    solve numDigits = maximum (allPalindromes $ nums numDigits)

    euler4 :: Integer
    euler4 = solve 3
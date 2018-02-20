module Euler9 ( euler9 ) where
    pow2 x = x * x
    choises = [[a,b,c] | a <- [1 .. 334],
                         b <- [2 .. 1000],
                         c <- [334 .. 1000],
                         a < b,
                         b < c,
                         a + b + c == 1000,
                         (a * a) + (b * b) == (c * c)
                         ]
    solve = product . head $ choises
    euler9 :: Integer
    euler9 = solve
module Euler5 ( euler5 ) where
    import Hasky (primesTo)

    divByAll :: Integer -> [Integer] -> Bool
    divByAll n []        = False
    divByAll n [d]       = n `rem` d == 0
    divByAll n (d : ds)
        | n `rem` d /= 0 = False
        | n `rem` d == 0 = divByAll n ds
        
    increment = product (primesTo 20)
    solve = solve' increment where
        solve' n 
            | divByAll n $ reverse [1 .. 20]  = n
            | otherwise             = solve' (n + increment)

    euler5 :: Integer
    euler5 = solve
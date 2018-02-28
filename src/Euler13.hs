module Euler13 ( euler13 ) where
    import Hasky(ints, digits)

    solve s = read ((take 10 . show . sum . ints) $ s) :: Integer
 
    euler13 :: String -> Integer
    euler13 =  solve
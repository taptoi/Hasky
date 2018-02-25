module Euler8 ( euler8 ) where
    import Data.Char (isHexDigit, digitToInt)
    import Hasky (digits)
    listsOf n xs = filter (\a -> length a == n) $ listsOf' xs 
                    where 
                        listsOf' [] = []
                        listsOf' [d] = []
                        listsOf' (d:ds) = take n ([d] ++ ds) : listsOf' ds
    solve = maximum . map product . listsOf 13 . digits

    euler8 :: String -> Integer
    euler8 = solve
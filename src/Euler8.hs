module Euler8 ( euler8 ) where
    import Data.Char (isHexDigit, digitToInt)
    digits s = map (toInteger . digitToInt) $ filter isHexDigit s
    listsOf n xs = filter (\a -> length a == n) $ listsOf' xs 
                    where 
                        listsOf' [] = []
                        listsOf' [d] = []
                        listsOf' (d:ds) = take n ([d] ++ ds) : listsOf' ds
    solve s = maximum $ map (foldl (*) 1) (listsOf 13 (digits s))

    euler8 :: String -> Integer
    euler8 s = solve s
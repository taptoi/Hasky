module Euler1 ( euler1 ) where
        import Data.List (union)
        euler1 :: Integer
        euler1 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]
        --euler1 = sum (union [3,6..999] [5,10..999])
    
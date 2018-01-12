module Euler1 ( euler1 ) where
        import Data.List (union)
        euler1 :: Int
        euler1 = sum (union [3,6..999] [5,10..999])
    
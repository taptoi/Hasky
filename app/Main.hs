module Main where

import System.Environment
import System.Exit  
import System.Clock
import Data.Time.Clock
import Control.Exception
import Formatting
import Formatting.Clock
import Hasky
import Euler1

main = getArgs >>= parse >>= putStr . display

display  = unlines . lines

toMilliSeconds :: TimeSpec -> Double
toMilliSeconds ts = fromIntegral((toNanoSecs ts)) / (10^6)

benchmarkWithResult f = 
        do  
            start <- getTime ProcessCPUTime
            evaluate f
            end <- getTime ProcessCPUTime
            let time = toMilliSeconds (diffTimeSpec end start)
            putStrLn ("Result: " ++ show (f))
            putStrLn ("Time: " ++ (show time) ++ "ms")

parse [] = getContents
parse ["euler1"] = benchmarkWithResult euler1 >> exit
parse [_] = Main.die
                

usage   = putStrLn "Usage: hasky [command]"
version = putStrLn "Hasky 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
module Main where

import Control.Monad
import System.Environment
import System.Exit  
import System.Clock
import Data.Time.Clock
import Control.Exception
import Formatting
import Formatting.Clock
import Hasky
import Euler1
import Euler2
import Euler67

main = getArgs 
        >>= parseArgs 
        >>= mapToExpression
        >>= computeWithBenchMark 
        >>= putStr

parseArgs :: [String] -> IO String
parseArgs [] = getContents
parseArgs ["euler1"] = do return "euler1"
parseArgs ["euler2"] = do return "euler2"
parseArgs ["euler67"] = do return "euler67"
parseArgs [_] = Main.exitFailure

mapToExpression :: String -> IO Integer
mapToExpression s = 
    case s of
        "euler1" ->
            do return euler1
        "euler2" ->
            do return euler2
        "euler67" -> 
            do 
                input <- readFile "triangle.txt"
                let parseInput = map (map read . words) . lines
                let inp = reverse (parseInput input)
                return (euler67 inp)
        _ -> return (-1)

computeWithBenchMark :: Integer -> IO String
computeWithBenchMark f = 
    do  
        start <- getTime ProcessCPUTime
        evaluate f
        end <- getTime ProcessCPUTime
        let time = toMilliSeconds (diffTimeSpec end start)
        let res = "Result: " ++ show (f) ++ "\nTime: " ++ (show time) ++ "ms\n"
        return res

toMilliSeconds :: TimeSpec -> Double
toMilliSeconds ts = fromIntegral((toNanoSecs ts)) / (10^6)

exitFailure   = exitWith (ExitFailure 1)
exitSuccess   = exitWith ExitSuccess
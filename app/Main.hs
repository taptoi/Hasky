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
import Euler3
import Euler4
import Euler5
import Euler6
import Euler7
import Euler8
import Euler9
import Euler10
import Euler11
import Euler12
import Euler13
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
parseArgs ["euler3"] = do return "euler3"
parseArgs ["euler4"] = do return "euler4"
parseArgs ["euler5"] = do return "euler5"
parseArgs ["euler6"] = do return "euler6"
parseArgs ["euler7"] = do return "euler7"
parseArgs ["euler8"] = do return "euler8"
parseArgs ["euler9"] = do return "euler9"
parseArgs ["euler10"] = do return "euler10"
parseArgs ["euler11"] = do return "euler11"
parseArgs ["euler12"] = do return "euler12"
parseArgs ["euler13"] = do return "euler13"
parseArgs ["euler67"] = do return "euler67"
parseArgs [_] = Main.exitFailure

mapToExpression :: String -> IO Integer
mapToExpression s = 
    case s of
        "euler1" ->
            do return euler1
        "euler2" ->
            do return euler2
        "euler3" ->
            do return euler3
        "euler4" ->
            do return euler4
        "euler5" ->
            do return euler5
        "euler6" ->
            do return euler6
        "euler7" ->
            do return euler7
        "euler8" ->
            do  
                input <- readFile "euler8Input.txt"
                return (euler8 input)
        "euler9" ->
            do return euler9
        "euler10" ->
            do return euler10
        "euler11" ->
            do
                input <- readFile "euler11Input.txt"
                return (euler11 input)
        "euler12" ->
            do
                return euler12
        "euler13" ->
            do
                input <- readFile "euler13Input.txt"
                return (euler13 input)
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
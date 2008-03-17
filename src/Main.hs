module Main where

import System.IO
import System.Environment (getArgs)
import Evaluator
import Parser

main :: IO ()
main = do
    fn <- getArgs
    if length fn > 0
        then do
            runFile (fn !! 0)
        else do
            putStrLn "Starting REPL..."
            hSetBuffering stdout LineBuffering
            repl



repl :: IO ()
repl = loopUntil (\x -> x == ":q") (prompt "lipl> ") evalAndPrint

{-
repl = do
    input <- prompt "lipl> "
    if input == ":q"
        then putStrLn "bye"
        else do
            putStrLn $ interpretSingle input
            repl
-}


loopUntil pred prompt action = do
    input <- prompt
    if pred input
    then
        return ()
    else
        do
            action result
            loopUntil pred prompt action


prompt :: String -> IO String
prompt p = do
    putStr p
    hFlush stdout
    getLine

runFile :: FilePath -> IO ()
runFile fn = do
    prog <- readFile fn
    putStrLn $ interpretMultiple prog


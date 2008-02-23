module Main where

import System.IO
import System.Environment (getArgs)
import Evaluator

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    repl

repl :: IO ()
repl = do
    input <- prompt "lipl> "
    if input == ":q"
        then putStrLn "bye"
        else do
            putStrLn $ interpret input
            repl

prompt :: String -> IO String
prompt p = do
    putStr p
    hFlush stdout
    getLine


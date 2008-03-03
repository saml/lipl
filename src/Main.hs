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

runFile :: FilePath -> IO ()
runFile fn = do
    prog <- readFile fn
    putStrLn $ interpret prog

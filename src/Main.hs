module Main where

import System.IO
import System.Environment (getArgs)
import Lipl.Repl
import Lipl.Interpreter

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    if null args
        then Lipl.Repl.repl
        else Lipl.Interpreter.run (head args)



module Main where

import System.IO
import System.Environment (getArgs)
import qualified Lipl.Repl as Lipl
import qualified Lipl.Interpreter as Lipl

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    if null args
        then Lipl.repl
        else Lipl.run (head args)



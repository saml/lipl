module Main where

import System.IO
import System.Environment (getArgs)
import MainUtils

main = do
    hSetBuffering stdout NoBuffering
    createBaseDir
    fn <- getArgs
    if length fn > 0
        then do
            run (do
                loadPrelude
                loadFile (fn !! 0)
                return ())
        else do
            hSetBuffering stdout LineBuffering
            putStrLn "Starting REPL..."
            run (loadPrelude >> repl)



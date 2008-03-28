module Main where

import System.IO
import System.Directory
import System.Environment (getArgs)
import qualified Data.Map as Map
import qualified Control.Monad.Error as E
import qualified Control.Monad.State as S
import qualified Control.Monad.Trans as T
import qualified Control.Monad as M
import Control.Applicative ((<$>))

import Evaluator
import Parser
import LangData

run wrap = S.runStateT (E.runErrorT (runWrap wrap)) nullEnv

prelude = "test.lipl"

loadPrelude = runAndPrint (loadFile prelude)

runAndPrint action = do
    msg <- action
    T.liftIO $ putStrLn msg

main = do
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

println s = T.liftIO $ putStrLn s

repl :: Wrap ()
repl = do
    line <- M.liftM (unwords . words) $ T.liftIO $ prompt "lipl> "
    if length line == 0
        then
            repl
        else
            processInput line

processInput line =
    case (head . words) line of
        ":q" -> do
            println "bye"
            return ()
        ":e" -> do
            println "Current Environment"
            printEnv
            repl
        ":pop" -> do
            println "Pop Environment"
            popEnv
            printEnv
            repl
        ":c" -> do
            println "Clear Environment"
            clearEnv
            printEnv
            repl
        ":l" -> do
            result <- loadFile $ (head . tail . words) line
            println result
            repl
        ":r" -> do
            clearEnv
            println "Cleared Environment"
            result <- loadFile $ (head . tail . words) line
            println result
            repl
        otherwise -> do
            result <- (show <$> parseAndEval line)
                `E.catchError`
                (\e -> return (show e))
            println result
            repl

loadFile fileName = do
    result <- (interpret fileName >>
        return (fileName ++ " loaded"))
        `E.catchError`
        (\e -> return (show e))
        --(\e -> E.throwError e)
    return result


printEnv :: Wrap ()
printEnv = do
    env <- getEnv
    T.liftIO $ putStrLn (showEnv env)

interpret :: FilePath -> Wrap ()
interpret file = do
    isValidFile <- T.liftIO $ doesFileExist file
    if isValidFile
        then
            do
                prog <- T.liftIO $ readFile file
                parseAndEvalMultiple file prog
                return ()
        else
            E.throwError $ DefaultErr $ "can't find file: " ++ file

parseAndEval input = case parseSingle input of
    Left err -> E.throwError $ ParseErr err
    Right val -> eval val

parseAndEvalMultiple fn input = case parseMultiple fn input of
    Left err -> E.throwError $ ParseErr err
    Right vals -> do
        evaled <- M.mapM eval vals
        return Null

prompt :: String -> IO String
prompt p = do
    putStr p
    hFlush stdout
    getLine

{-
loopUntil pred prompt action = do
    input <- prompt
    if pred input
        then
            return ()
        else
            do
                action input
                loopUntil pred prompt action
-}

module Main where

import System.IO
import System.Directory
import System.FilePath
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
import TCheck
import Error
import TIMonad
import EvalMonad
import REPLMonad
import Type
import Settings
import PosMonad

--run wrap = runTIT (E.runErrorT (runREPL wrap)) initialSubst
run action = S.runStateT (runPosT (S.runStateT (
    runEvalT (runTIT (E.runErrorT (runREPL action)) initialSubst 0))
    nullEnv)) initialPos

baseDir = do
    home <- getHomeDirectory
    return $ joinPath [home, sBASEDIR]

createBaseDir = do
    d <- baseDir
    exists <- doesDirectoryExist d
    if exists then return () else createDirectory d

findPrelude = do
    exists <- doesFileExist sPRELUDE
    if exists
        then
            return sPRELUDE
        else
            do
                d <- baseDir
                return $ joinPath [d, sPRELUDE]

loadPrelude = do
    --fname <- findPrelude `catch` (\e -> return "")
    let fname = sPRELUDE
    loadFile fname
--loadPrelude = runAndPrint (loadFile sPRELUDE)

runAndPrint action = do
    msg <- action
    T.liftIO $ putStrLn msg

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

println s = T.liftIO $ putStrLn s

repl = do
    line <- M.liftM (unwords . words) $ T.liftIO $ prompt (sLANGNAME ++ "> ")
    if length line == 0
        then
            repl
        else
            processInput line

processInput line =
    case (head . words) line of
        ":?" -> do
            println (unlines [":? help"
                , ":s current type environment"
                , ":q quit"
                , ":e current environment"
                , ":c clear environment"
                , ":l <file> load <file>"
                , ":r <file> load <file> on clean environment"])
            repl
        ":s" -> do
            println "Current Type Environment"
            printSubst
            repl
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
            clearEnv
            println "Environment cleared"
            printEnv
            clearSubst
            println "Type environment cleared"
            repl
        ":l" -> do
            result <- loadFile $ (head . tail . words) line
            println result
            repl
        ":r" -> do
            clearEnv
            println "Environment cleared"
            clearSubst
            println "Type environment cleared"
            loadPrelude
            result <- loadFile $ (head . tail . words) line
            println result
            repl
        otherwise -> do
            result <- (show <$> rollBackOnErr (parseAndEval line))
                `E.catchError`
                (\e -> return (show e))
            println result
            repl


loadFile fileName = do
    result <- (interpret fileName >>
        return (fileName ++ " loaded"))
        `E.catchError`
        (\e -> return (show e))
    return result

printSubst = do
    s <- getSubst
    T.liftIO $ putStrLn (showSubst s)

printEnv = do
    env <- getEnv
    T.liftIO $ putStrLn (showEnv env)

interpret file = do
    isValidFile <- T.liftIO $ doesFileExist file
    if isValidFile
        then
            do
                prog <- T.liftIO $ readFile file
                rollBackOnErr (parseAndEvalMultiple file prog)
                return ()
        else
            do
                pos <- getSourcePos
                E.throwError $ Err pos ("can't find file: " ++ file)

parseAndEval input = case parseSingle input of
    Left err -> do
        pos <- getSourcePos
        E.throwError $ Err pos (show err)
    Right val -> do
        t <- typeInfer val
        println ("type: " ++ show t)
        evaluate val

{-
parseAndEval input = case parseSingle input of
    Left err -> E.throwError $ ParseErr err
    Right val -> eval val
-}

parseAndEvalMultiple fn input = case parseMultiple fn input of
    Left err -> do
        pos <- getSourcePos
        E.throwError $ Err pos (show err)
    Right vals -> do
        M.mapM (\val -> (do
            t <- typeInfer val
            return t)) vals
        M.mapM evaluate vals
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
ev input = case parseSingle input of
    Left err -> error (show err)
    Right v -> runEvalMonad (eval v)
-}
{-
ev input = do
    ((_,_,result),_) <- run (do
        v <- parseAndEval input
        return v)
    return result
-}

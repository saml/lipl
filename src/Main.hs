module Main where

import System.IO
import System.Environment (getArgs)
import qualified Control.Monad.Error as E
import qualified Control.Monad.State as S
import qualified Control.Monad.Trans as T
import qualified Control.Monad as M
import Evaluator
import Parser
import LangData
import qualified Data.Map as Map


{-
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

evalAndPrint :: String -> IO ()
evalAndPrint input = do
    case runEval (evalString input) of
    putStrLn $ show (evalString input)
-}

{-
    case evalString input of
        Left err -> putStrLn $ show err
        Right val -> putStrLn $ show val
-}

nullEnv = [Map.empty]

main = S.runStateT (E.runErrorT (runWrap repl)) nullEnv

repl :: Wrap ()
repl = do
    line <- T.liftIO $ prompt "lipl> "
    case line of
        ":q" -> return ()
        otherwise -> do
            ((M.liftM show (parseAndEval line))
                `E.catchError` (return . show)) >>= (T.liftIO . putStrLn)
            repl

--result <- E.catchError (parseAndEval line) (\e -> return e)
--result <- parseAndEval line

parseAndEval input = case parseSingle input of
    Left err -> E.throwError $ ParseErr err
    Right val -> eval val

{-
runEvalAndPrint env wrapped = case runEval env wrapped of
    (Left err, st) -> putStrLn $ show err
    (Right val, st) -> putStrLn $ show val

evalAndPrint env input = runEvalAndPrint env $ parseAndEval input
-}



--repl = nullEnv
--    >>= loopUntil (\x -> x == ":q") (prompt "lipl> ") . evalAndPrint

{-
main = do
    input <- prompt "lipl> "
    if input == ":q"
        then
            return ()
        else
            do
                wrapVal <- parseAndEval input
                let (val, st) = runEval wrapVal
-}

    --return $ fst
    --    $ runEval [Map.empty] $ (eval . unwrap . parseSingle) input
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
                action input
                loopUntil pred prompt action


prompt :: String -> IO String
prompt p = do
    putStr p
    hFlush stdout
    getLine

runFile :: FilePath -> IO ()
runFile fn = do
    prog <- readFile fn
    --putStrLn $ interpretMultiple prog
    putStrLn prog

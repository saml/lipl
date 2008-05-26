=============
MainUtils.lhs
=============

.. sectnum::
.. contents::

MainUtils implements various actions used in Main program.

.. sc:: lhs

> module MainUtils (run, module MainUtils) where
>
> import System.IO
> import System.Directory
> import System.FilePath
> import qualified Control.Exception as Ex
> import qualified System.Environment as Sys
> import qualified Control.Monad.Error as E
> import qualified Control.Monad.State as S
> import qualified Control.Monad.Trans as T
> import qualified Control.Monad as M
> import qualified Text.ParserCombinators.Parsec as P
> import Text.ParserCombinators.Parsec.Pos (SourceName)
>
> import Evaluator
> import Parser
> import LangData
> import TCheck
> import Error
> import TIMonad
> import EvalMonad
> import REPLMonad
> import Type
> import Settings
> import PosMonad
> import Utils
>
> liplPath = do
>     path <- Ex.handle (\e -> return "") (Sys.getEnv sPATH)
>     base <- baseDir
>     return (splitOn searchPathSeparator path ++ [base])

liplPath returns list of directories defined in LIPLPATH
environment variable plus baseDir (``~/.lipl``).

.. sc:: lhs

> baseDir = do
>     home <- getHomeDirectory
>     return $ joinPath [home, sBASEDIR]

baseDir returns ``~/.lipl``.

.. sc:: lhs

> createBaseDir = do
>     d <- baseDir
>     exists <- doesDirectoryExist d
>     if exists then return () else createDirectory d

createBaseDir creats ``~/.lipl`` if it does not exist.

.. sc:: lhs

> getValidFile l = do
>     yes <- mapM doesFileExist l
>     getFirst (zip yes l)
>     where
>         getFirst [] = return ""
>         getFirst ((True, x):_) = return x
>         getFirst (_:xs) = getFirst xs

getValidFile takes a list of file paths and
returns the first file that actually exists.
If nothing in the list exists, "" is returned.

.. sc:: lhs

> findPrelude = do
>     exists <- doesFileExist sPRELUDE
>     if exists
>         then
>             return sPRELUDE
>         else
>             do
>                 paths <- liplPath
>                 let fs = map (\x -> joinPath [x, sPRELUDE]) paths
>                 f <- getValidFile fs
>                 return f

findPrelude returns location of ``core.lipl`` file.
It searches for ``core.lipl`` in the following order:

1. in the current working directory.
1. in directories listed in LIPLPATH environment variable.
1. in ``~/.lipl``.

If ``core.lipl`` is not found, "" is returned.

.. sc:: lhs

> loadPrelude = do
>     fname <- T.liftIO findPrelude
>     runAndPrint (loadFile fname)

loadPrelude loads ``core.lipl``.
It can fail when ``core.lipl`` cannot be found.

.. sc:: lhs

> runAndPrint action = do
>     msg <- action
>     T.liftIO $ putStrLn msg

runAndPrint runs an action that returns String and prints the String.

.. sc:: lhs

> println s = T.liftIO $ putStrLn s

println is short hand for ``liftIO . putStrLn``.

.. sc:: lhs

> repl :: REPL ()
> repl = (do
>     line <- M.liftM (unwords . words)
>         $ T.liftIO $ prompt (sLANGNAME ++ "> ")
>     if length line == 0
>         then
>             repl
>         else
>             processInput line) `E.catchError`
>                 (\e -> do
>                     println (show e)
>                     repl)

repl is read eval print loop. It runs in REPL monad.

.. sc:: lhs

> loadFile fileName = do
>     result <- (interpret fileName >>
>         return (fileName ++ " loaded"))
>     return result

loadFile loads a LIPL file.

.. sc:: lhs

> interpret :: FilePath -> REPL ()
> interpret file = do
>     isValidFile <- T.liftIO $ doesFileExist file
>     if isValidFile
>         then
>             do
>                 prog <- T.liftIO $ readFile file
>                 rollBackOnErr (parseAndEvalMultiple file prog)
>                 return ()
>         else E.throwError $ Default ("can't find file: " ++ file)

interpret interprets a LIPL file
by evaluating one LIPL expression at a time, expanding
environment in REPL monad as needed (for example, function
definition registers new function name to the environment).
In case of error, REPL monad's state is restored.
Only when all expressions in the file are successfully evaluated,
REPL monad's state is committed.

.. sc:: lhs

> processInput line =
>     case (head . words) line of
>         ":?" -> do
>             println (unlines [":? help"
>                 , ":s current type environment"
>                 , ":q quit"
>                 , ":e current environment"
>                 , ":c clear environment"
>                 , ":l <file> load <file>"
>                 , ":r <file> load <file> on clean environment"])
>             repl
>         ":s" -> do
>             println "Current Type Environment"
>             printSubst
>             repl
>         ":q" -> do
>             println "bye"
>             return ()
>         ":e" -> do
>             println "Current Environment"
>             printEnv
>             repl
>         ":pop" -> do
>             println "Pop Environment"
>             popEnv
>             printEnv
>             repl
>         ":c" -> do
>             clearEnvs
>             println "Environment cleared"
>             printEnv
>             clearSubst
>             println "Type environment cleared"
>             repl
>         ":l" -> do
>             result <- loadFile $ (head . tail . words) line
>             println result
>             repl
>         ":r" -> do
>             clearEnvs
>             println "Environment cleared"
>             clearSubst
>             println "Type environment cleared"
>             loadPrelude
>             result <- loadFile $ (head . tail . words) line
>             println result
>             repl
>         otherwise -> do
>             result <- (show `fmap` rollBackOnErr (parseAndEval line))
>             println result
>             repl

processInput parses various repl commands (:l, :c, :s, ...etc.
commands start with :)
and performs various actions according to the given commands.
When the user input was not a repl command, it considers
the input to be a LIPL expression and evaluates the input
(also type inference is performed).

.. sc:: lhs

> printSubst = do
>     s <- getSubst
>     T.liftIO $ putStrLn (showSubst s)
>
> printEnv = do
>     env <- getEnvs
>     T.liftIO $ putStrLn (showEnvs env)

printSubst prints current Subst (variable type information).
printEnv prints current Env (variable value information).
printEnv does not print about built-in functions while printSubst
does.

.. sc:: lhs

> parseAndEval :: String -> REPL Val
> parseAndEval input = case parseSingle input of
>     Left err -> do
>         E.throwError $ Default (show err)
>     Right val -> do
>         t <- typeInfer val
>         println ("type: " ++ show t)
>         evaluate val
>
> parseAndEvalMultiple :: SourceName -> String -> REPL Val
> parseAndEvalMultiple fn input = case parseMultiple fn input of
>     Left err -> do
>         E.throwError $ Default (show err)
>     Right vals -> do
>         M.mapM (\val -> (do
>             t <- typeInfer val
>             return t)) vals
>         M.mapM evaluate vals
>         return Null

parseAndEval considers the input to be a single LIPL expression.
It infers type of the expression and evaluates it, printing
both type and result upon success.
parseAndEvalMultiple considers the input to be a series of LIPL
expressions. It infers type of each expression and evaluates it,
while modifying state of REPL monad (for example,
function definition registers both type information and
value information for the function name).

Because of how parseAndEvalMultiple is implemented
(using mapM), a LIPL file is executed from top to bottom.
So, one must define a function before it can be used.

.. sc:: lhs

> prompt :: String -> IO String
> prompt p = do
>     putStr p
>     hFlush stdout
>     getLine

prompt prompts user for input and returns what user entered.

========
Main.lhs
========

.. sectnum::
.. contents::

Main module is LIPL interpreter entry point.

.. sc:: lhs

> module Main where
>
> import qualified Control.Monad.Error as E
>
> import System.IO
> import System.Environment (getArgs)
> import MainUtils
>
> main = do
>     hSetBuffering stdout NoBuffering
>     createBaseDir
>     fn <- getArgs
>     if length fn > 0
>         then do
>             run ((do
>                 loadPrelude
>                 loadFile (fn !! 0)
>                 return ()) `E.catchError` (\e -> println (show e)))
>         else do
>             hSetBuffering stdout LineBuffering
>             putStrLn "Starting REPL..."
>             run (loadPrelude >> repl)

main action is the entry point.
It creates ``~/.lipl`` if it doesn't exist.
When arguments is passed to the interpreter,
the first argument is considered to be path to a LIPL file
and the file is loaded (executed).
Otherwise, repl is started.

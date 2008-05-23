=============
REPLMonad.lhs
=============

.. sectnum::
.. contents::

REPLMonad defines REPL monad that supports
useful actions: getSubst, putEnv, ...
to build the LIPL interpreter (including read eval print loop).

.. sc:: lhs

> {-# LANGUAGE GeneralizedNewtypeDeriving
>     , FlexibleInstances #-}
>
> module REPLMonad where
>
> import qualified Control.Monad.Trans as T
> import qualified Control.Monad.Error as E
> import qualified Control.Monad.State as S
> import qualified Control.Monad.Writer as W
> import qualified Control.Monad.Reader as R
>
> import TIMonad
> import EvalMonad
> import Error
> import TCheck
> import PosMonad
>
> newtype REPL a = REPL {
>     runREPL :: TIT (EvalT (PosT (E.ErrorT Err IO))) a
>     } deriving (Monad, Functor
>         , MonadTI, MonadEval, MonadPos
>         , E.MonadError Err, T.MonadIO)

REPL has TI, Eval, Pos, Error, IO monad::

    +------+
    | TI   | provides MonadTI: getSubst, newId,...
    |------+
    | Eval | provides MonadEval: getEnv, putEnvs, ...
    |------|
    | Pos  | provides MonadPos: getSourcePos and setSourcePos
    |------|
    | Error| provides MonadError: throwError and catchError
    |------|
    | IO   | provides IO actions: putStrLn, getLine, ...
    +------+

So, in REPL, any of those actions can be used.

.. sc:: lhs

> rollBackOnErr action = do
>     envs <- getEnvs
>     s <- getSubst
>     n <- getN
>     pos <- getSourcePos
>     result <- action `E.catchError` (\e -> do
>         putEnvs envs
>         putSubst s
>         putN n
>         setSourcePos pos
>         E.throwError e)
>     return result

rollBackOnErr caches current state of the interpreter:
Envs, Subst, Int, SourcePos, ...
and executes the given action.
In case of error, cached state is restored and the error
re-thrown.

